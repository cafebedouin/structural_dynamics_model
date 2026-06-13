% ============================================================================
% CONSTRAINT STORY: aneyoshi_stone_commitment__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: aneyoshi_stone_commitment__behavioral_competence_reading
 *   human_readable: Aneyoshi Stone Commitment: Behavioral Competence Reading (Land-Use Regulation)
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   In the 1930s, a tsunami devastated a coastal village in Iwate Prefecture,
 *   Japan, killing residents who had built in low-lying zones. Survivors
 *   erected a stone monument bearing a directive: build above this elevation.
 *   For 78 years, that stone retained operational force as a land-use rule.
 *   Villagers, builders, and landowners made concrete placement decisions
 *   constrained by the stone's embedded guidance—not because a government
 *   enforcer mandated it, but because the stone encoded ancestral knowledge
 *   and the community transmitted that knowledge intergenerationally. In
 *   2011, when the Tōhoku tsunami struck, the settlement pattern was exactly
 *   as the stone had prescribed: houses stood above the inundation line. The
 *   constraint's behavioral efficacy across eight decades, culminating in the
 *   2011 survival outcome, is the empirical claim this reading instantiates.
 *   This is ONE reading of a contested kernel (aneyoshi_stone_commitment):
 *   the behavioral_competence_reading. A sibling reading
 *   (commemorative_husk_reading) reads the stone as a symbolic memorial whose
 *   regulatory power had decayed to cultural observance with no actual
 *   behavioral force on land use. This reading asserts the opposite: the
 *   stone functioned as a live constraint on location decisions, and that
 *   function causally contributed to the 2011 survival outcome. The
 *   ε-invariance test: measuring the stone as a behavioral regulator yields
 *   very low extractiveness (0.12 at interval end) because the constraint
 *   extracts nothing from the constrained parties—no rents, no
 *   redistribution, no enrichment for an agenda-setter. Measuring the stone
 *   as a ceremonial memorial yields a different ε (higher theater component,
 *   lower behavioral binding). These are two different constraints. This file
 *   instantiates the behavioral_competence reading cleanly.
 *
 * KEY AGENTS:
 *   - village_residents_1933_2011: Community members whose building placement decisions were constrained by the stone's elevation directive; beneficiaries of the constraint's protective function
 *   - village_construction_actors: Builders and landowners who enforced the stone's rule through local coordination (checking placement, avoiding low-lying sites)
 *   - intergenerational_transmission_community: The family and community memory structures that carried the stone's directive across 78 years without institutional reinforcement
 *   - seismic_science_authorities: Post-2011 researchers who verified the stone's elevation guidance against tsunami inundation data and paleoseismic records
 *   - municipal_authorities: Regional government observers who recognized the stone's rule as evidence of effective long-term disaster planning only after 2011
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__behavioral_competence_reading, 0.12).
domain_priors:suppression_score(aneyoshi_stone_commitment__behavioral_competence_reading, 0.08).
domain_priors:theater_ratio(aneyoshi_stone_commitment__behavioral_competence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__behavioral_competence_reading, rope).
narrative_ontology:human_readable(aneyoshi_stone_commitment__behavioral_competence_reading, "Aneyoshi Stone Commitment: Behavioral Competence Reading (Land-Use Regulation)").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__behavioral_competence_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__behavioral_competence_reading, 'f99df25e-1934-4068-a6d1-058e2502fedb').
narrative_ontology:cs_kernel_codification('f99df25e-1934-4068-a6d1-058e2502fedb', fixed_text).
narrative_ontology:cs_authority_grounding('f99df25e-1934-4068-a6d1-058e2502fedb', practice).
narrative_ontology:cs_interpretation_layer_present('f99df25e-1934-4068-a6d1-058e2502fedb').
narrative_ontology:cs_reading_relation('f99df25e-1934-4068-a6d1-058e2502fedb', aneyoshi_stone_commitment__commemorative_husk_reading, forecloses).
narrative_ontology:cs_axiom('f99df25e-1934-4068-a6d1-058e2502fedb', foundational, stone_constrains_land_use_behavior).
narrative_ontology:cs_axiom_status(stone_constrains_land_use_behavior, holdable).
narrative_ontology:cs_axiom_grounding('f99df25e-1934-4068-a6d1-058e2502fedb', stone_constrains_land_use_behavior, empirically_contingent).
narrative_ontology:cs_axiom('f99df25e-1934-4068-a6d1-058e2502fedb', foundational, intergenerational_knowledge_transmission_via_material_substrate).
narrative_ontology:cs_axiom_status(intergenerational_knowledge_transmission_via_material_substrate, holdable).
narrative_ontology:cs_axiom_grounding('f99df25e-1934-4068-a6d1-058e2502fedb', intergenerational_knowledge_transmission_via_material_substrate, instrumental).
narrative_ontology:cs_reference_frame('f99df25e-1934-4068-a6d1-058e2502fedb', ancestral_hazard_knowledge_embodied_in_stone).
narrative_ontology:cs_drift_state('f99df25e-1934-4068-a6d1-058e2502fedb', contemporary_post_2011_recognition, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('f99df25e-1934-4068-a6d1-058e2502fedb', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, village_residents).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, village_residents_1933_2011).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, intergenerational_transmission_community).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__behavioral_competence_reading, local_knowledge_embedded_in_material_culture).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__behavioral_competence_reading, intergenerational_transmission_of_disaster_avoidance).
narrative_ontology:constraint_vindicates(aneyoshi_stone_commitment__behavioral_competence_reading, non_coercive_behavioral_coordination_via_material_substrate).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lived within the village boundaries across eight decades. The stone's injunction to build above a certain elevation directly protected them: the 2011 Tōhoku tsunami reached the prescribed line but did not cross into the elevation zone where houses stood. Their interpretation of the stone's directive as actionable land-use guidance, passed down through family memory and community practice, determined building placement decisions. They did not experience the stone as externally imposed; it encoded a locally-derived rule from ancestors who had witnessed tsunami damage.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, village_residents_1933_2011, beneficiary,
    moderate, biographical, constrained, local).

% Builders, landowners, and heads of household who made concrete location decisions for new structures across the 78-year interval. They inherited the stone's directive as a local norm and enforced it through coordination—checking placement against the stone's elevation guidance, rejecting low-lying sites even when they offered economic or convenience advantages. No formal coercion mechanism existed; the constraint was maintained through shared understanding that the stone embodied tested knowledge.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, village_construction_actors, agenda_setter,
    moderate, biographical, constrained, local).

% The village community spanning generations (oral tradition, family memory, local lore) maintained the stone's directive across 78 years without institutional reinforcement. Grandparents told grandchildren where the boundary was; the stone itself stood as a material reminder. This intergenerational coordination function is the constraint's deepest beneficiary—the system perpetuated itself through social memory and symbolic reference rather than external enforcement.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, intergenerational_transmission_community, beneficiary,
    organized, civilizational, arbitrage, local).

% Municipal government, prefectural planners, and land-use authorities after WWII operated within national frameworks that did not formally recognize the stone's directive. They observed but did not formally codify or enforce the local elevation rule. After 2011, they recognized the stone's embedded instruction as evidence of effective long-term disaster planning and began studying it as a model for future coastal zone regulation.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, municipal_authorities_postwar, observer,
    institutional, generational, analytical, regional).

% Geophysicists, oceanographers, and disaster researchers post-2011 studied the stone as a document of embedded empirical knowledge. They verified that the stone's elevation directive was consistent with tsunami inundation modeling and paleoseismic records. From this seat, the stone represents a sophisticated, intuitive grasp of coastal hazard geography encoded in material form—evidence of how pre-modern local knowledge embedded itself in durable cultural artifacts.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, seismic_science_authorities, observer,
    institutional, generational, analytical, national).

% Residents who might have wanted to build on lower-elevation, economically advantageous sites but could not because the stone's directive constrained their placement options. They were not explicitly organized against the rule; rather, they internalized it as part of the local landscape. Some of this group survived 2011 because the constraint held; others who might have preferred lower sites were satisfied with elevated placement after the constraint proved predictive.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, coastal_residents_competing_locations, excluded,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_commitment__behavioral_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(aneyoshi_stone_commitment__behavioral_competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Encodes and transmits a tested land-use rule across generations without institutional overhead: build above this elevation to avoid tsunami inundation. The stone solves the coordination problem of intergenerational knowledge transmission about local disaster geography by making the rule durable, visible, and tied to a specific material landmark that persists across decades of population turnover.
% TRANSFER_FUNCTION: The constraint transfers no rents or resources between parties. Instead, it transfers knowledge and behavioral guidance from ancestors to descendants, from the 1930s understanding of local hazard geography to building placement decisions in 2011. What moves is not extraction but information: the empirical claim that 'stay above this line' is embedded in the stone's material position.
% ABSENT_VOICES: Coastal residents who preferred lower-elevation building sites (better access to water, economic advantages, social convenience) but were guided away by the stone's directive. They are not an organized opposition—the constraint was internalized as part of local culture—but their preferences were structurally suppressed by conformity to the inherited rule. Post-2011, most of this group became supporters, as the constraint's predictive accuracy vindicated the inherited knowledge.
% DISAPPEARANCE_RATIONALE: If the stone's directive had not constrained building location decisions, the village would have expanded downslope into lower-elevation zones where land was cheaper and water access was easier. The 2011 tsunami inundation would have reached those structures; mortality would have been substantially higher. The constraint's disappearance would have meant settlement in zones that proved catastrophically vulnerable. The village's actual survival is causally linked to compliance with the stone's elevation rule.
% FOUNDING_PROBLEM: A tsunami in the 1930s killed villagers who had built in low-lying zones. Survivors wanted to encode a rule that would prevent future generations from making the same error: stay above the line marked by this stone. The problem was how to make a rule durable across 78 years of population turnover, shifting land values, and institutional discontinuity (wartime disruption, postwar administrative change) without relying on written law or external enforcement.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by local oral history (village elders, family narratives preserved across generations), by the stone's own inscription (dated to post-1930s period, bearing a warning in classical Japanese), and by seismic paleontologists and disaster historians who documented pre-modern tsunami deposits consistent with the 1930s event. The 2011 Tōhoku Commission and international disaster-response researchers verified that the stone's elevation guidance aligned with actual tsunami inundation. Corroboration comes from outside the village beneficiary set: geophysicists, government historians, and international observers all confirmed the founding problem as historically real and the stone's rule as empirically sound.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__behavioral_competence_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(aneyoshi_stone_commitment__behavioral_competence_reading, 'none', 1).

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
 *   Extractiveness is very low (0.12) because the stone transfers no rents between parties. The constraint imposes a cost on low-elevation land development (foregone economic advantage) but collects nothing for an agenda-setter. The village's construction actors enforced the rule, but they were enforcing it on themselves and each other—a coordination function, not extraction. Suppression is minimal (0.08) because the stone did not need to coerce compliance; it encoded knowledge the community believed was true (and whose accuracy was proven 78 years later). The theater_ratio rises modestly over time (0.05 to 0.15) as post-war institutional structures began to render the stone ceremonial—municipal planners documented it, schools taught it as history, the stone became increasingly a symbol of tradition rather than a live regulatory mechanism. But the behavioral constraint remained in place. Accessibility_collapse is very high (0.92) because once the stone's elevation directive was understood, alternatives (building below the line) were not suppressed coercively but were simply logically unavailable within the community's understanding: if the stone marks a tsunami boundary, building below it was recognized as irrational. Resistance is near zero (0.05) because no organized opposition to the constraint emerged—the rule was internalized as ancestral wisdom. The measurement series track a constraint that remains stable in its core behavioral function (base_extractiveness barely rises; suppression remains minimal) while the theatrical/ceremonial component increases as post-war institutions begin to frame the stone as heritage.
 *
 * PERSPECTIVAL GAP:
 *   The village construction actors and residents experience the stone as a beneficial constraint: it encodes knowledge they believe is true and protects them. The seismic science seat verifies the stone as encoding accurate empirical knowledge. The municipal authorities initially treated the stone as a curious artifact until 2011, when they recognized it as evidence of sophisticated disaster planning. No seat experiences this constraint as extraction or coercion; the perspectival gap is not between beneficiary and victim but between those who live inside the constraint (experience it as natural local knowledge) and those who observe it from outside (recognize it as an effective governance mechanism). The engine computes all seats' directionality from the structural data: village residents and construction actors are beneficiaries (d near 0.0), municipal/science observers are analytical. No victim seat exists because the constraint extracts nothing.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declaration (village_residents, future_generations) is structurally justified: the constraint protects them from tsunami hazard without extracting rents or imposing coercive surveillance. No victims are declared because the constraint's costs (foregone low-elevation building options) are borne by the beneficiary set itself and are experienced as rational tradeoffs, not as extraction. The village's intergenerational coordination function is a secondary beneficiary: the transmission of knowledge across 78 years is the core benefit. Exit options for residents are constrained (local settlement patterns, family land, agricultural dependence) but the constraint itself does not generate that dependence—it navigates within it. Directionality across all seats sits in the beneficiary range (d ≤ 0.3) because the constraint coordinates behavior without creating asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exemplifies the opposite of mandatrophy: the founding problem (how to prevent tsunami deaths by encoding a land-use rule for future generations) remains live through 2011. The constraint persists not because of institutional inertia but because its founding purpose was achieved (preventing settlement in dangerous zones) and remains aligned with the village's actual interests. The modest rise in theater_ratio reflects increasing ceremonial recognition of the stone post-2011, but the behavioral function—the original mandate—never atrophied. The 2011 outcome (houses stood above the inundation line, confirming 78 years of constraint compliance) constitutes active vindication rather than decay. A genuine mandatrophy reading would require that the stone's directive had been internalized as pure tradition while the founding problem (tsunami hazard) became irrelevant—but the 2011 tsunami proved the founding problem catastrophically real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_vs_ceremonial_decay,
    'Did the stone''s regulatory force remain live and behavioral throughout the 78-year interval, or did it decay into ceremonial observance while actual land-use decisions were driven by economic and administrative factors independent of the stone''s directive?',
    'Longitudinal analysis of building-location decisions at multiple time points: examine construction records, family narratives, municipal planning documents, and material evidence of settlement patterns in 1940s, 1960s, 1980s, and 2000s; measure correlation between distance from stone''s elevation line and building placement; compare against economic incentives (land cost, water access) that would have favored lower-elevation sites.',
    'If behavioral constraint was live throughout: the constraint is a rope (coordination via material cultural substrate) with very low extractiveness. If decay occurred: the constraint transitions from rope to piton (inert ceremonial artifact maintained theatrically). The 2011 causal outcome (survival following constraint compliance) is evidence for the behavioral reading but does not resolve the question of when decay, if any, occurred.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_vs_ceremonial_decay, empirical, 'Whether the stone retained live behavioral force or had attenuated to ceremonial observance.').

omega_variable(
    intergenerational_transmission_mechanism,
    'How was the stone''s directive transmitted and enforced across 78 years without written law, institutional codification, or external enforcement authority? What social structures carried the rule?',
    'Ethnographic documentation of family memory transmission, oral tradition in schools and community gatherings, relationship between the stone as physical landmark and social memory, role of elder-to-youth instruction, mechanisms by which community members corrected deviations from the rule.',
    'Understanding the transmission mechanism clarifies whether the constraint''s persistence was due to genuine behavioral coordination (shared belief in the rule''s validity) or to institutional accident (the rule persisted despite lack of understanding). High clarity on transmission would strengthen the behavioral_competence reading; opacity would raise the commemorative_husk reading''s plausibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_transmission_mechanism, empirical, 'Social and cognitive structures that carried the stone''s land-use directive intergenerationally.').

omega_variable(
    founding_problem_empirical_accuracy,
    'Was the 1930s tsunami the primary hazard the stone was designed to protect against, and did the stone''s elevation guidance accurately capture the empirical hazard zone?',
    'Paleoseismic analysis, tsunami deposit mapping, historical records of 1930s event inundation extent, geological survey of tsunami runup zone relative to stone''s position. Comparison of stone''s prescribed elevation against modern tsunami modeling for comparable events.',
    'If the stone accurately captured the hazard zone: the constraint represents sophisticated local knowledge embedded in material form, and the behavioral reading gains empirical grounding. If the stone''s guidance was historically contingent or luck-based: the constraint might be better read as ceremonial embodiment of hazard anxiety rather than behavioral competence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_empirical_accuracy, empirical, 'Empirical validation of the stone''s elevation guidance against geological and historical hazard data.').

omega_variable(
    reading_foreclosure_or_coexistence,
    'Do the behavioral_competence and commemorative_husk readings logically foreclose each other, or can they coexist as different framing of the same constraint held by different parties?',
    'Logical analysis: the behavioral reading asserts ''the stone constrained actual land-use decisions''; the commemorative reading asserts ''the stone functioned as ceremonial memorial with no behavioral force.'' These are contradictory claims about the same interval (1933–2011). If a third party holds both readings simultaneously (e.g., ''the stone was ceremonially important AND constrained land use''), the readings coexist; if the village population held one or the other but not both, they foreclose each other within that population''s framework.',
    'If readings foreclose: only one can be true, and the 2011 outcome is decisive evidence for the behavioral reading. If readings coexist: the two communities (behavioral believers, commemorative symbolists) exist in parallel, and the constraint''s type-signature may diverge per seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_or_coexistence, conceptual, 'Whether the two readings are logically contradictory (foreclose) or can be held simultaneously by different parties (coexist).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__behavioral_competence_reading, 1930, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1930, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1930, 0.05).
narrative_ontology:measurement_basis(aney_tr_t1930, observed).
narrative_ontology:measurement(aney_tr_t1960, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement_basis(aney_tr_t1960, projected).
narrative_ontology:measurement(aney_tr_t1980, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement_basis(aney_tr_t1980, projected).
narrative_ontology:measurement(aney_tr_t2000, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 2000, 0.14).
narrative_ontology:measurement_basis(aney_tr_t2000, projected).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 2011, 0.15).
narrative_ontology:measurement_basis(aney_tr_t2011, observed).

% Extraction over time
narrative_ontology:measurement(aney_be_t1930, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1930, 0.08).
narrative_ontology:measurement_basis(aney_be_t1930, observed).
narrative_ontology:measurement(aney_be_t1960, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1960, 0.1).
narrative_ontology:measurement_basis(aney_be_t1960, projected).
narrative_ontology:measurement(aney_be_t1980, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1980, 0.11).
narrative_ontology:measurement_basis(aney_be_t1980, projected).
narrative_ontology:measurement(aney_be_t2000, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 2000, 0.12).
narrative_ontology:measurement_basis(aney_be_t2000, projected).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 2011, 0.12).
narrative_ontology:measurement_basis(aney_be_t2011, observed).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t1930, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 1930, 0.03).
narrative_ontology:measurement_basis(aney_su_t1930, observed).
narrative_ontology:measurement(aney_su_t1960, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 1960, 0.04).
narrative_ontology:measurement_basis(aney_su_t1960, projected).
narrative_ontology:measurement(aney_su_t1980, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 1980, 0.06).
narrative_ontology:measurement_basis(aney_su_t1980, projected).
narrative_ontology:measurement(aney_su_t2000, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 2000, 0.07).
narrative_ontology:measurement_basis(aney_su_t2000, projected).
narrative_ontology:measurement(aney_su_t2011, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 2011, 0.08).
narrative_ontology:measurement_basis(aney_su_t2011, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_commitment__behavioral_competence_reading, information_standard).
narrative_ontology:boltzmann_floor_override(aneyoshi_stone_commitment__behavioral_competence_reading, 0.05).
narrative_ontology:affects_constraint(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_stone_commitment__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% The aneyoshi_stone_commitment kernel decomposes into two structurally distinct readings: (1) behavioral_competence_reading (this file) — the stone functioned as a live land-use constraint with measurable behavioral effects across 78 years, very low extractiveness, coordination via material cultural substrate; (2) commemorative_husk_reading (sibling) — the stone had decayed into symbolic memorial observance with negligible behavioral force on actual land-use decisions. The two readings emit different constraint types from the same material artifact because their ε values differ fundamentally: behavioral measurement yields near-zero extraction (no rents transferred, only knowledge coordinated), while ceremonial measurement yields higher theater-ratio and lower behavioral binding. Per ε-invariance principle (DP-001), these are two constraints, not two viewpoints on one constraint. Both stories are linked via this network field and share kernel_id; each instantiates its own reading_id and independent cs_structure with reading_relations mapping to the sibling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
