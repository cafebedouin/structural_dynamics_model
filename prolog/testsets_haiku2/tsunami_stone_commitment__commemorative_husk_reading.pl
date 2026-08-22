% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsunami_stone_commitment__commemorative_husk_reading, []).

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
 *   constraint_id: tsunami_stone_commitment__commemorative_husk_reading
 *   human_readable: Tsunami Stone Commitment as Commemorative Husk
 *   domain: social/disaster_anthropology/commitment_systems
 *
 * SUMMARY:
 *   In Japan and other tsunami-prone regions, ancestors carved stone markers
 *   above the highest runup zones of past tsunamis—monuments encoding
 *   ecological knowledge across generations. Over the modern period
 *   (especially post-WWII), these stones were reinterpreted from behavioral
 *   instructions ('do not build below this mark') to commemorative artifacts
 *   ('historical record of a past disaster'). This reading instantiates the
 *   constraint as it operates under the commemorative framing: the stone
 *   persists materially but has lost prescriptive force. Coastal development
 *   interests benefit from this reframing because it divorces the stone from
 *   development restrictions. Future coastal residents—who inherit densely
 *   built environments in vulnerable zones—bear the extraction: they occupy
 *   the developed landscape the stone's decay enabled. The 2011 Tōhoku
 *   tsunami empirically tested the two readings: survivors in communities
 *   that maintained the stone's behavioral authority (like Anai in Iwate
 *   Prefecture) had better survival and lower loss rates than those in zones
 *   that ignored the markers. This constraint story models the commemorative
 *   reading: what extraction operates when the stone is treated as artifact
 *   rather than instruction. The sibling behavioral_competence_reading models
 *   the counter-reading where the stone retains live operative force.
 *
 * KEY AGENTS:
 *   - coastal_development_interests: benefit from reframing stone as memorial; treat it as divorced from operational constraints
 *   - intergenerational_coastal_population: future residents who inherit the built landscape enabled by the stone's decay
 *   - traditional_knowledge_holders: elders whose authority to interpret the stone was displaced by modernist institutions
 *   - modernist_institutional_order: the epistemic and economic system that privileges technical infrastructure over cultural memory
 *   - memory_institutions: museums and heritage organizations that institutionalize the commemorative framing
 *   - geological_record: evidence of where tsunamis actually ran up, recorded in the stone's placement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__commemorative_husk_reading, 0.82).
domain_priors:suppression_score(tsunami_stone_commitment__commemorative_husk_reading, 0.71).
domain_priors:theater_ratio(tsunami_stone_commitment__commemorative_husk_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(tsunami_stone_commitment__commemorative_husk_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__commemorative_husk_reading, piton).
narrative_ontology:human_readable(tsunami_stone_commitment__commemorative_husk_reading, "Tsunami Stone Commitment as Commemorative Husk").
narrative_ontology:topic_domain(tsunami_stone_commitment__commemorative_husk_reading, "social/disaster_anthropology/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__commemorative_husk_reading, 'd01aeddd-cb91-4274-9861-64bda1202f86').
narrative_ontology:cs_kernel_codification('d01aeddd-cb91-4274-9861-64bda1202f86', fixed_text).
narrative_ontology:cs_authority_grounding('d01aeddd-cb91-4274-9861-64bda1202f86', extraction).
narrative_ontology:cs_interpretation_layer_present('d01aeddd-cb91-4274-9861-64bda1202f86').
narrative_ontology:cs_reading_relation('d01aeddd-cb91-4274-9861-64bda1202f86', tsunami_stone_commitment__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('d01aeddd-cb91-4274-9861-64bda1202f86', foundational, commemorative_primacy_over_behavioral).
narrative_ontology:cs_axiom_status(commemorative_primacy_over_behavioral, holdable).
narrative_ontology:cs_axiom_grounding('d01aeddd-cb91-4274-9861-64bda1202f86', commemorative_primacy_over_behavioral, conventional).
narrative_ontology:cs_axiom('d01aeddd-cb91-4274-9861-64bda1202f86', foundational, modernist_infrastructure_supersedes_traditional_knowledge).
narrative_ontology:cs_axiom_status(modernist_infrastructure_supersedes_traditional_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('d01aeddd-cb91-4274-9861-64bda1202f86', modernist_infrastructure_supersedes_traditional_knowledge, empirically_contingent).
narrative_ontology:cs_reference_frame('d01aeddd-cb91-4274-9861-64bda1202f86', post_war_modernization_framework).
narrative_ontology:cs_drift_state('d01aeddd-cb91-4274-9861-64bda1202f86', contemporary_anthropocene, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d01aeddd-cb91-4274-9861-64bda1202f86', '').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__commemorative_husk_reading, tsunami_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__commemorative_husk_reading, coastal_development_interests).
narrative_ontology:constraint_victim(tsunami_stone_commitment__commemorative_husk_reading, future_coastal_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__commemorative_husk_reading, modernist_institutional_order).
narrative_ontology:constraint_victim(tsunami_stone_commitment__commemorative_husk_reading, intergenerational_coastal_population).
narrative_ontology:constraint_victim(tsunami_stone_commitment__commemorative_husk_reading, traditional_knowledge_holders).
narrative_ontology:constraint_vindicates(tsunami_stone_commitment__commemorative_husk_reading, modernity_displaces_traditional_knowledge).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Economic actors—developers, municipal planners, port authorities—who benefit from unrestricted coastal development. They treat the stone inscription as a historical artifact divorced from operational warning function. By reinterpreting it as cultural memory rather than behavioral instruction, they escape the constraint it once imposed on building location and density. They maintain it as theater (museum piece, tourist attraction) while disarming its prescriptive force.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, coastal_development_interests, beneficiary,
    institutional, biographical, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(tsunami_stone_commitment__commemorative_husk_reading, coastal_development_interests, agenda_setter).

% Future residents of the coastal zone who inherit the developed landscape the inscription's decay enabled. They bear the extraction: they occupy built environments in tsunami-vulnerable locations, having lost the behavioral anchor—and the collective memory—that the stone inscription once maintained. They cannot exit the geography; they inherit compressed exit windows (pre-disaster evacuation, post-disaster recovery) and elevated risk.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, intergenerational_coastal_population, payer,
    powerless, generational, trapped, regional).

% Elders and families who transmitted the stone's meaning and the behavioral norms it encoded. They observe the inscription's decay from operative norm to museum artifact—a loss of intergenerational channel and symbolic authority. Their expertise is treated as quaint rather than operational, their warnings dismissed as superstition. They pay through cultural displacement and loss of transmission authority.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, traditional_knowledge_holders, payer,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(tsunami_stone_commitment__commemorative_husk_reading, traditional_knowledge_holders, observer).

% The broader epistemic and economic system that privileges quantified risk models, engineering controls, and market-driven development over traditional ecological knowledge encoded in monuments. The stone's decay vindicates the modernist premise that technical infrastructure (seawalls, building codes, warning systems) replaces cultural memory. Benefits from treating the inscription as artifact rather than instruction.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, modernist_institutional_order, beneficiary,
    institutional, civilizational, analytical, global).

% Museums, heritage organizations, and documentation projects that preserve the stone inscription as historical/cultural record. They set its framings: historical marker, tourist site, scholarly object. By institutionalizing it in the commemorative rather than behavioral register, they perform the extraction's maintenance work—turning warning into witness.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, memory_institutions, agenda_setter,
    organized, biographical, mobile, regional).

% Those whose lives will be shaped by the next tsunami. They are structurally absent from the reading's conversation—they have no voice in whether coastal development proceeds or whether the stone's prescriptive authority is restored. They pay the immediate cost (injury, death, displacement, property loss) that the stone's decay enabled.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, future_disaster_survivors, excluded,
    powerless, immediate, trapped, local).

% The observable sequence of tsunamis and the stone's placement above the runup zone—a non-agent entity recording whether the ancestor who placed it understood the hazard. Included for completeness: geological evidence of the stone's competence.
narrative_ontology:constraint_stakeholder(tsunami_stone_commitment__commemorative_husk_reading, geological_record, observer,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(tsunami_stone_commitment__commemorative_husk_reading, geological_record).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tsunami_stone_commitment__commemorative_husk_reading, coastal_development_interests).
narrative_ontology:fixing_cost_class(tsunami_stone_commitment__commemorative_husk_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The stone's original function: coordinates coastal population to avoid settlement zones where tsunamis run up, via a durable material marker encoding traditional ecological knowledge. This coordination solved the problem of intergenerational memory loss—ensuring that tsunami-hazard knowledge persisted even if oral transmission weakened.
% TRANSFER_FUNCTION: Extraction of future safety for present development freedom: present development interests extract the ability to build unrestricted in vulnerable zones; they transfer that cost forward to future residents who inherit concentrated population in the runup zone and compressed evacuation windows. Under the commemorative reading, the stone transfers cultural legitimacy to modernist infrastructure narratives—it becomes evidence that 'we have evolved past needing superstitious markers; our engineering is better.'
% ABSENT_VOICES: Future coastal residents who will experience the next tsunami have no seat at the table. Disaster survivors post-event become momentary voices calling for building restrictions, but pre-event (the structural moment) they are excluded. Traditional knowledge practitioners are present as observers/payers, not as authorities whose reading would be binding.
% DISAPPEARANCE_RATIONALE: If the stone's prescriptive authority were restored—if it were re-read as behavioral instruction rather than commemorative artifact—coastal development would face explicit constraints. Building location, density, and land-use planning would reorganize around the stone's marking of safe zones. The present economic system would rearrange as constrained; the future system would rearrange toward lower risk. The 2011 Tōhoku tsunami proved that some stones in Japan WERE read correctly—those that survived the runup became evidence that ancestors had positioned markers above hazard zones. The constraint's disappearance (or restoration) maps directly to development patterns.
% FOUNDING_PROBLEM: Ancestor coastal residents faced repeated tsunamis and lacked durable records of where the waves reached. They carved markers—some above runup zones—to preserve knowledge for descendants. The founding problem: intergenerational transmission of hazard information in the absence of written records, scientific institutions, or central archives.
% FOUNDING_PROBLEM_CORROBORATION: Geological and historical record confirms the original problem: pre-modern populations depended on such markers and had high memory-loss risk. Modern corroboration: the 2011 Tōhoku tsunami proved some marks were positioned correctly. But contemporary institutional players (development, memory institutions, modernist governance) testify that the problem is 'solved' by engineering codes, seismic networks, and warning systems—making the stone's behavioral function obsolete. The testimony about obsolescence comes entirely from the beneficiary seats. No independent voice from future populations exists (they are excluded); survivors who testify post-event are not heard pre-event.
narrative_ontology:disappearance_verdict(tsunami_stone_commitment__commemorative_husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(tsunami_stone_commitment__commemorative_husk_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tsunami_stone_commitment__commemorative_husk_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tsunami_stone_commitment__commemorative_husk_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tsunami_stone_commitment__commemorative_husk_reading, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsunami_stone_commitment__commemorative_husk_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tsunami_stone_commitment__commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tsunami_stone_commitment__commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises over the interval (0.35 → 0.82) as coastal development accelerates and the stone's reframing becomes institutionalized. Theater ratio climbs even faster (0.15 → 0.68), indicating that maintaining the stone's commemorative status—restoration efforts, museum exhibits, heritage plaques—becomes increasingly performative work disconnected from any actual behavioral restraint. Suppression is moderate and stable (0.42 → 0.71) because the stone's decay does not require active coercion; the reframing is self-sustaining through epistemic authority (modernist institutions treat traditional knowledge as obsolete) and structural exclusion (future victims have no voice in present development). The constraint is piton-shaped: it was once a live rope (behavioral norm coordinating coastal settlement), has atrophied into performance (theatrical preservation), persists by inertia and institutional memory work, and extracts from those it no longer protects. No concentrated beneficiary maintains it against pressure—development interests benefit but do not invest in the stone; memory institutions maintain it as culture-work, not as functional governance.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (intergenerational coastal population) and the beneficiary seat (coastal development interests) compute radically different constraint types. From the development perspective, the stone is a beautiful historical preservation (rope-like: coordination around shared cultural heritage). From the future resident perspective, it is a monument to their own vulnerability—a marker that should have constrained the building that now surrounds them (snare-like: extraction enabled by reframing their vulnerability as antiquity). The engine derives this divergence from the structural data: different power atoms, different exit options, different positions in the beneficiary/victim arrays. The authored claim (piton) reflects the reading's assessment that the stone's original function has atrophied and its current state is mostly performance—but that assessment is independent of what each seat experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal development interests sit as beneficiaries with institutional power and high exit optionality (they can shift to other development zones if needed, though they prefer the valuable coastal locations). Their directionality is low (they subsidize from the constraint—it enables their preferred land use). Intergenerational coastal population is the structural target: powerless, trapped geographically, identity-locked to their region, facing compressed exit windows post-disaster. Their directionality is high (the constraint extracts from them). Traditional knowledge holders sit intermediate: moderate power, constrained exit (their cultural authority is displaced but they remain in the region), and they pay through loss of transmission authority and epistemic standing. Memory institutions are agenda-setters (they frame the stone) but they do not collect extraction—they perform maintenance work that enables the extraction. Under the commemorative reading, no power atom can be assigned an override (the structural derivation from beneficiary/victim + exit determines d throughout).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (intergenerational transmission of hazard knowledge) was real and the stone was a genuine coordination solution. By the commemorative reading, the founding problem is 'dead'—deemed solved by modernist infrastructure. But the 2011 tsunami provides empirical evidence that the founding problem is not actually dead; it resurfaces whenever modern warning systems fail or when evacuation capacity is overwhelmed. The stone's decay represents a false resolution: the problem is declared solved rather than actually solved. This is the mandatrophy signature: the constraint persists by reframing what it solves, not by solving it. The constraint is extracted from future residents under the guise of cultural preservation—the stone is maintained as a museum piece (theater ratio 0.68) rather than as an operative warning. Classification as piton (degraded/inertial) rather than as snake is appropriate because no actor actively extracts; the extraction is passive—the stone's loss of behavioral authority enables development that would not occur if the behavioral norm persisted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_vs_commemorative_reading_contest,
    'Is the tsunami stone a degraded behavioral constraint (husk of operative norm) or a always-already commemorative artifact (historical marker that never carried strong behavioral force)?',
    'Ethnographic documentation of pre-modern coastal populations'' actual compliance with stone markers; analysis of settlement patterns relative to stone placements; oral histories from knowledge holders about whether the stone WAS enforced as behavioral instruction or always treated as historical record.',
    'If behavioral reading is correct: the constraint was extracted via reframing (active reinterpretation displacing operative norm). If commemorative reading was always dominant: the extraction is less about degradation and more about modernist displacement of traditional knowledge as the framing device itself. Type may shift from piton toward tangled_rope if behavioral reading is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_vs_commemorative_reading_contest, empirical, 'Core reading distinction: operative norm vs. historical marker from origin').

omega_variable(
    extraction_via_reframing_vs_technological_substitution,
    'Does the stone''s decay as operative norm represent extraction (deliberate reframing by development interests to enable unrestricted building) or technological displacement (modernist institutions genuinely believed engineering made it obsolete)?',
    'Institutional history: were development decisions explicitly made by ignoring the stone''s prescriptions, or was the stone''s authority eroded through non-controversial epistemic shift? Did development actors consciously choose to override the constraint, or did they inherit a post-stone governance landscape where behavioral compliance was no longer salient?',
    'Explicit extraction via reframing → snare signature. Epistemic displacement without explicit override → piton signature. This reading assumes piton; the omega documents the possibility of stronger extraction classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_via_reframing_vs_technological_substitution, conceptual, 'Intentional extraction vs. unintended technological supersession').

omega_variable(
    modernist_epistemic_authority_over_traditional_knowledge,
    'Is the victory of modernist infrastructure narratives (engineering codes, seismic networks) over stone-based ecological knowledge a legitimate epistemological shift, or a power displacement where institutional authority substituted technical apparatus for traditional competence?',
    'Comparative effectiveness: post-stone jurisdictions (relying on engineering) vs. jurisdictions where stone-based norms persisted. The 2011 Tōhoku tsunami provides partial evidence—some Japanese communities that preserved oral traditions and respected stone markers survived better than those that ignored them. But confounds exist (building codes also improved, seismic networks existed). Resolution requires counterfactual: what would outcomes have been if the stone''s behavioral authority had been maintained alongside engineering?',
    'This is a preference omega, not empirical: whether the displacement constitutes extraction depends on whether modernism is granted epistemic authority to override. If granted: extractiveness is partially legitimate coordination cost. If denied: extractiveness is pure knowledge displacement. Classification may shift between readings of the kernel depending on which epistemic frame is endorsed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(modernist_epistemic_authority_over_traditional_knowledge, preference, 'Epistemic authority: modernist infrastructure vs. traditional ecological knowledge').

omega_variable(
    future_disaster_visibility_exclusion,
    'Is the fact that future disaster survivors have no voice in the commitment-system reading a structural feature (they are temporally absent from deliberation) or a manifestation of deliberate exclusion (present actors consciously silence future-oriented voices)?',
    'Institutional analysis: do present governance structures include future-impact assessments and intergenerational representation in development decisions, or are future residents categorically absent from planning? Post-disaster interviews: do survivors report that pre-event warnings about traditional knowledge existed but were ignored, or were such warnings genuinely unavailable?',
    'If temporally absent: the extraction is a feature of present-biased institutions, not deliberate suppression. If deliberately excluded: the constraint carries a hidden enforcement arm (keeping future voices out of pre-event deliberation). Classification may sharpen toward snare if deliberate exclusion is documented.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_disaster_visibility_exclusion, empirical, 'Whether future residents are structurally or deliberately absent from the constraint''s governance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__commemorative_husk_reading, 0, 140).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsun_tr_t0, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(tsun_tr_t0, observed).
narrative_ontology:measurement(tsun_tr_t20, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 20, 0.28).
narrative_ontology:measurement_basis(tsun_tr_t20, observed).
narrative_ontology:measurement(tsun_tr_t40, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(tsun_tr_t40, observed).
narrative_ontology:measurement(tsun_tr_t60, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 60, 0.55).
narrative_ontology:measurement_basis(tsun_tr_t60, observed).
narrative_ontology:measurement(tsun_tr_t80, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 80, 0.64).
narrative_ontology:measurement_basis(tsun_tr_t80, observed).
narrative_ontology:measurement(tsun_tr_t100, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 100, 0.66).
narrative_ontology:measurement_basis(tsun_tr_t100, observed).
narrative_ontology:measurement(tsun_tr_t120, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 120, 0.68).
narrative_ontology:measurement_basis(tsun_tr_t120, observed).
narrative_ontology:measurement(tsun_tr_t140, tsunami_stone_commitment__commemorative_husk_reading, theater_ratio, 140, 0.68).
narrative_ontology:measurement_basis(tsun_tr_t140, observed).

% Extraction over time
narrative_ontology:measurement(tsun_be_t0, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(tsun_be_t0, observed).
narrative_ontology:measurement(tsun_be_t20, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement_basis(tsun_be_t20, observed).
narrative_ontology:measurement(tsun_be_t40, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(tsun_be_t40, observed).
narrative_ontology:measurement(tsun_be_t60, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 60, 0.71).
narrative_ontology:measurement_basis(tsun_be_t60, observed).
narrative_ontology:measurement(tsun_be_t80, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 80, 0.78).
narrative_ontology:measurement_basis(tsun_be_t80, observed).
narrative_ontology:measurement(tsun_be_t100, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 100, 0.81).
narrative_ontology:measurement_basis(tsun_be_t100, observed).
narrative_ontology:measurement(tsun_be_t120, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 120, 0.82).
narrative_ontology:measurement_basis(tsun_be_t120, observed).
narrative_ontology:measurement(tsun_be_t140, tsunami_stone_commitment__commemorative_husk_reading, base_extractiveness, 140, 0.82).
narrative_ontology:measurement_basis(tsun_be_t140, observed).

% Suppression requirement over time
narrative_ontology:measurement(tsun_su_t0, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(tsun_su_t0, observed).
narrative_ontology:measurement(tsun_su_t20, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement_basis(tsun_su_t20, observed).
narrative_ontology:measurement(tsun_su_t40, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 40, 0.59).
narrative_ontology:measurement_basis(tsun_su_t40, observed).
narrative_ontology:measurement(tsun_su_t60, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 60, 0.65).
narrative_ontology:measurement_basis(tsun_su_t60, observed).
narrative_ontology:measurement(tsun_su_t80, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 80, 0.69).
narrative_ontology:measurement_basis(tsun_su_t80, observed).
narrative_ontology:measurement(tsun_su_t100, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 100, 0.7).
narrative_ontology:measurement_basis(tsun_su_t100, observed).
narrative_ontology:measurement(tsun_su_t120, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 120, 0.71).
narrative_ontology:measurement_basis(tsun_su_t120, observed).
narrative_ontology:measurement(tsun_su_t140, tsunami_stone_commitment__commemorative_husk_reading, suppression_requirement, 140, 0.71).
narrative_ontology:measurement_basis(tsun_su_t140, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsunami_stone_commitment__commemorative_husk_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(tsunami_stone_commitment__commemorative_husk_reading, 0.12).
narrative_ontology:affects_constraint(tsunami_stone_commitment__commemorative_husk_reading, tsunami_stone_commitment__behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% The tsunami_stone_commitment kernel supports two structurally distinct constraints: the behavioral_competence_reading (stone as live norm; mountain-to-rope signature) and the commemorative_husk_reading (stone as artifact; piton signature, high extraction). The readings decompose the kernel because they instantiate different ε values: behavioral reading has low ε (genuine coordination cost), commemorative reading has high ε (extraction enabled by reframing). No single constraint can hold both ε values; each reading is its own story. They are linked by affects_constraints because the commemorative reading's institutional dominance suppresses the behavioral reading's operative force—if the behavioral reading were to gain authority, coastal development would reorganize.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
