% ============================================================================
% CONSTRAINT STORY: commemorative_husk_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_commemorative_husk_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: commemorative_husk_reading
 *   human_readable: Aneyoshi Stone Directive as Commemorative Husk (Lost Behavioral Force)
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   The Aneyoshi stone directive represents a catastrophe-driven
 *   institutional memory mechanism: a stone monument inscribed with a
 *   behavioral rule (do not build below this line) that once constrained
 *   coastal settlement patterns in response to tsunami risk. This constraint
 *   story instantiates ONE READING of a contested kernel — the
 *   commemorative-husk reading — in which the stone's behavioral force has
 *   decayed during the inter-catastrophe period (the decades or centuries
 *   between major tsunami events) while its commemorative function has
 *   intensified. The stone is now primarily a memorial artifact and heritage
 *   site rather than an active constraint on settlement decisions. This
 *   reading coexists with the behavioral-competence reading (the stone still
 *   constrains settlement through institutional memory) as a live dispute
 *   among different stakeholders. The commemorative-husk reading is the
 *   dominant institutional interpretation: heritage institutions maintain the
 *   stone's symbolic value through ritual and education, while development
 *   interests benefit from the decay of its behavioral force. The constraint
 *   exhibits high extractiveness (0.68) because the stone's loss of
 *   behavioral force suppresses tsunami risk mitigation capacity while
 *   coastal development interests gain from the removal of settlement
 *   restrictions. The theater ratio (0.81) reflects that the stone's primary
 *   function is now performative — annual ceremonies, school visits, and
 *   historical commemoration — rather than functional risk governance. The
 *   suppression (0.72) is high because the stone's degraded authority creates
 *   false confidence in existing risk awareness while actual behavioral
 *   constraints have decayed.
 *
 * KEY AGENTS:
 *   - Future Coastal Residents: Primary victims (powerless/trapped) — inherit geographic vulnerability without institutional memory that would trigger evacuation protocols
 *   - Coastal Development Interests: Primary beneficiaries (institutional/arbitrage) — gain from the removal of settlement restrictions as the stone's behavioral force decays
 *   - Disaster Risk Reduction Practitioners: Secondary victims (moderate/constrained) — recognize the stone's original function but cannot enforce it without institutional backing
 *   - Local Heritage and Memory Institutions: Institutional actors (organized/constrained) — maintain the stone's commemorative function through ritual and education; depend on its symbolic value for funding
 *   - National Disaster Management Authority: Institutional actor (institutional/constrained) — inherits the stone's directive but lacks enforcement capacity during inter-catastrophe period
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the stone's decay as an immutable feature of institutional memory rather than an engineered suppression
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commemorative_husk_reading, 0.68).
domain_priors:suppression_score(commemorative_husk_reading, 0.72).
domain_priors:theater_ratio(commemorative_husk_reading, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commemorative_husk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(commemorative_husk_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(commemorative_husk_reading, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commemorative_husk_reading, snare).
narrative_ontology:human_readable(commemorative_husk_reading, "Aneyoshi Stone Directive as Commemorative Husk (Lost Behavioral Force)").
narrative_ontology:topic_domain(commemorative_husk_reading, "disaster_anthropology/institutional_memory/land_use_governance").

domain_priors:requires_active_enforcement(commemorative_husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commemorative_husk_reading, '6eca6b2d-68f4-4070-bdaf-d063253bf80f').
narrative_ontology:cs_kernel_codification('6eca6b2d-68f4-4070-bdaf-d063253bf80f', fixed_text).
narrative_ontology:cs_authority_grounding('6eca6b2d-68f4-4070-bdaf-d063253bf80f', extraction).
narrative_ontology:cs_interpretation_layer_present('6eca6b2d-68f4-4070-bdaf-d063253bf80f').
narrative_ontology:cs_reading_relation('6eca6b2d-68f4-4070-bdaf-d063253bf80f', commemorative_husk_reading__behavioral_competence_reading, coexists_with).
narrative_ontology:cs_axiom('6eca6b2d-68f4-4070-bdaf-d063253bf80f', foundational, stone_function_commemorative_not_behavioral).
narrative_ontology:cs_axiom_status(stone_function_commemorative_not_behavioral, holdable).
narrative_ontology:cs_axiom_grounding('6eca6b2d-68f4-4070-bdaf-d063253bf80f', stone_function_commemorative_not_behavioral, empirically_contingent).
narrative_ontology:cs_axiom('6eca6b2d-68f4-4070-bdaf-d063253bf80f', secondary, inter_catastrophe_decay_natural_law).
narrative_ontology:cs_axiom_status(inter_catastrophe_decay_natural_law, holdable).
narrative_ontology:cs_axiom_grounding('6eca6b2d-68f4-4070-bdaf-d063253bf80f', inter_catastrophe_decay_natural_law, empirically_contingent).
narrative_ontology:cs_reference_frame('6eca6b2d-68f4-4070-bdaf-d063253bf80f', behavioral_directive_active).
narrative_ontology:cs_drift_state('6eca6b2d-68f4-4070-bdaf-d063253bf80f', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6eca6b2d-68f4-4070-bdaf-d063253bf80f', '2026-02-26T00:00:00Z').
narrative_ontology:cs_kernel_id(commemorative_husk_reading, aneyoshi_stone_directive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commemorative_husk_reading, coastal_development_interests).
narrative_ontology:constraint_victim(commemorative_husk_reading, tsunami_risk_mitigation_capacity).
narrative_ontology:constraint_victim(commemorative_husk_reading, future_coastal_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(commemorative_husk_reading, local_heritage_institutions).
narrative_ontology:constraint_victim(commemorative_husk_reading, disaster_risk_reduction_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inherit geographic vulnerability to tsunami risk in the Aneyoshi coastal zone. The stone once constrained settlement patterns through behavioral force; now it functions primarily as a memorial. Residents lack institutional memory of the directive and face no active enforcement of settlement restrictions. They bear the cost of reduced tsunami risk mitigation capacity without the protection the stone once provided. No exit from coastal exposure; no alternative settlement patterns enforced.
narrative_ontology:constraint_stakeholder(commemorative_husk_reading, future_coastal_residents, payer,
    powerless, generational, trapped, local).

% Benefit from the stone's loss of behavioral force. The stone once constrained profitable development in the coastal zone; as behavioral force decays, development restrictions are removed. Development interests can invest in the formerly-restricted zone, relocate capital, or pursue alternative projects. The stone's transition from directive to memorial removes institutional barriers to coastal development while heritage institutions maintain the stone's symbolic value, creating a win-win for development interests.
narrative_ontology:constraint_stakeholder(commemorative_husk_reading, coastal_development_interests, beneficiary,
    institutional, immediate, arbitrage, regional).

% Recognize the stone's original function as a behavioral constraint on settlement but cannot enforce it without institutional backing. They face resource barriers and institutional inertia in attempting to restore the stone's behavioral force. The stone's presence creates false confidence in existing risk awareness while its behavioral force has decayed. They can advocate for new protocols but face resistance from development interests and institutional inertia. Constrained exit: they depend on institutional support to enforce risk mitigation measures.
narrative_ontology:constraint_stakeholder(commemorative_husk_reading, disaster_risk_reduction_practitioners, payer,
    moderate, biographical, constrained, regional).

% Maintain the stone's commemorative function through ritual, education, and symbolic preservation. Annual ceremonies, school visits, historical plaques, and oral tradition transmission continue. The stone's symbolic value provides funding and legitimacy for heritage institutions. They depend on the stone's cultural authority for institutional survival. Constrained exit: they cannot abandon the stone without losing cultural legitimacy and institutional resources.
narrative_ontology:constraint_stakeholder(commemorative_husk_reading, local_heritage_institutions, agenda_setter,
    organized, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(commemorative_husk_reading, local_heritage_institutions, beneficiary).

% Inherits the stone's directive as part of the institutional landscape of disaster governance. The authority coordinates disaster response protocols and evacuation procedures. The stone's behavioral force has decayed during the inter-catastrophe period, but the authority lacks enforcement capacity to restore it. The authority's legitimacy depends partly on the stone's cultural authority, but the stone no longer delivers behavioral constraints. Constrained exit: the authority cannot simply abandon the stone without losing cultural legitimacy, but cannot enforce it without resources.
narrative_ontology:constraint_stakeholder(commemorative_husk_reading, national_disaster_management_authority, agenda_setter,
    institutional, generational, constrained, national).

% The abstract collective good of tsunami risk mitigation capacity is suppressed by the stone's loss of behavioral force. The stone once functioned as an institutional mechanism for constraining settlement in high-risk zones; as behavioral force decays, this protective function is lost. Risk mitigation capacity cannot organize or exit; it bears the full cost of the stone's degradation.
narrative_ontology:constraint_stakeholder(commemorative_husk_reading, tsunami_risk_mitigation_capacity, payer,
    powerless, civilizational, trapped, local).
narrative_ontology:stakeholder_non_agent(commemorative_husk_reading, tsunami_risk_mitigation_capacity).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The stone originally coordinated settlement patterns to reduce tsunami risk by constraining development in high-vulnerability zones. During the inter-catastrophe period, this coordination function has decayed while the stone's commemorative function has intensified. The constraint now coordinates heritage preservation and cultural memory rather than risk mitigation.
% TRANSFER_FUNCTION: The stone transfers risk exposure from development interests to future coastal residents. Development interests gain from the removal of settlement restrictions as the stone's behavioral force decays. Future residents inherit geographic vulnerability without the institutional memory or behavioral constraints that would trigger evacuation protocols. The stone also transfers cultural authority from disaster management to heritage institutions.
% ABSENT_VOICES: Voices absent from the commemorative-husk reading include: (1) future tsunami victims who cannot participate in current decision-making about settlement patterns; (2) indigenous communities whose original relationship to the stone and coastal zone may differ from current institutional interpretations; (3) disaster risk reduction practitioners whose expertise is marginalized by heritage institution dominance; (4) residents of other tsunami-vulnerable regions who might learn from the Aneyoshi case but are not included in current institutional memory maintenance.
% DISAPPEARANCE_RATIONALE: If the Aneyoshi stone directive disappeared overnight, the world would rearrange itself significantly. The stone's loss would remove a major symbol of cultural identity and heritage value for local communities. It would also remove a focal point for disaster risk awareness, even though the stone's behavioral force has already decayed. The disappearance would likely accelerate coastal development in the formerly-restricted zone and reduce institutional attention to tsunami risk mitigation. However, the world would not rearrange as dramatically as it would have if the stone's behavioral force were still intact — the stone's current function is primarily commemorative rather than protective.
% FOUNDING_PROBLEM: The Aneyoshi stone was inscribed to solve the problem of repeated tsunami catastrophe: how to constrain settlement in high-risk coastal zones through a durable institutional mechanism that would persist across generations and survive the loss of living memory of past disasters. The stone was designed to function as a behavioral directive that would constrain settlement patterns even during inter-catastrophe periods when no one alive had experienced a tsunami.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's death is attested by: (1) disaster risk reduction practitioners who recognize that the stone no longer constrains settlement decisions; (2) coastal development interests who have successfully developed in the formerly-restricted zone; (3) national disaster management authorities who acknowledge that the stone's behavioral force has decayed and new enforcement mechanisms are needed; (4) historical records showing that settlement has expanded into the zone the stone once constrained. The founding problem's status is NOT attested by heritage institutions, whose institutional interest lies in maintaining the stone's symbolic value regardless of its behavioral force.
narrative_ontology:disappearance_verdict(commemorative_husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(commemorative_husk_reading, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE COASTAL RESIDENTS (SNARE) — Trapped by the stone's loss of behavioral force. The directive once constrained settlement patterns; now it functions only as memorial. Residents inherit the geographic vulnerability without the institutional memory that would trigger evacuation protocols. No exit from coastal exposure; no alternative settlement patterns enforced. Maximum extraction: the stone's performative function (commemoration) masks its loss of protective function.
constraint_indexing:constraint_classification(commemorative_husk_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: DISASTER RISK REDUCTION PRACTITIONERS (SNARE) — Constrained by the stone's degraded authority. They recognize the directive's original function but cannot enforce it without institutional backing. The stone's presence creates false confidence in existing risk awareness while its behavioral force has atrophied. Constrained exit: they can advocate for new protocols but face resistance from development interests and institutional inertia.
constraint_indexing:constraint_classification(commemorative_husk_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: COASTAL DEVELOPMENT INTERESTS (ROPE) — Experience the stone as pure coordination: it marks a boundary that once constrained profitable development. As behavioral force decays, the stone becomes a memorial artifact rather than an enforcement mechanism. Development interests benefit from the transition from directive to commemoration. They have arbitrage options: develop in the formerly-restricted zone, relocate capital, or invest in sea walls. The constraint appears as coordination (the stone marks a zone) without extraction (the zone is no longer enforced).
constraint_indexing:constraint_classification(commemorative_husk_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: LOCAL HERITAGE AND MEMORY INSTITUTIONS (PITON) — Maintain the stone's commemorative function through ritual, education, and symbolic preservation. The institutional practice of remembrance persists despite the loss of behavioral force. Theater ratio is high: annual ceremonies, school visits, historical plaques, and oral tradition transmission continue, but the stone no longer shapes settlement decisions. The constraint is maintained through performative activity (heritage preservation) rather than functional necessity (risk mitigation). Constrained exit: institutions depend on the stone's symbolic value for funding and legitimacy.
constraint_indexing:constraint_classification(commemorative_husk_reading, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 5: NATIONAL DISASTER MANAGEMENT AUTHORITY (TANGLED ROPE) — Inherits the stone's directive as part of the institutional landscape but lacks enforcement capacity during the inter-catastrophe period. The authority coordinates disaster response protocols (genuine coordination function) while the stone's behavioral force decays (asymmetric extraction: the authority's legitimacy depends on the stone's authority, but the stone no longer delivers). Requires active enforcement to maintain the connection between the stone and evacuation protocols. Constrained exit: the authority cannot simply abandon the stone without losing cultural legitimacy, but cannot enforce it without resources.
constraint_indexing:constraint_classification(commemorative_husk_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the decay of institutional memory during inter-catastrophe periods is an immutable feature of human social organization: without repeated reinforcement through lived experience, behavioral directives lose force. The stone's transition from directive to memorial is a natural law of institutional memory, not a contingent failure. However, this perspective risks naturalizing what is actually a failure of institutional design and resource allocation — a false summit that obscures the extractive interests benefiting from the stone's degradation.
constraint_indexing:constraint_classification(commemorative_husk_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(commemorative_husk_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(commemorative_husk_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(commemorative_husk_reading, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(commemorative_husk_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(commemorative_husk_reading, TR),
    TR >= 0.70.

:- end_tests(commemorative_husk_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The stone's loss of behavioral force during the inter-catastrophe period creates asymmetric extraction: coastal development interests benefit from the removal of settlement restrictions while future residents bear the cost of reduced tsunami risk mitigation capacity. The extractiveness is not maximal (0.68 rather than 0.85+) because the extraction is mediated through institutional decay rather than active coercion — development interests benefit from the stone's degradation but do not directly enforce it. Suppression (0.72): High. The stone's transition from directive to memorial creates suppression through institutional inertia: the stone's presence creates false confidence in existing risk awareness (heritage institutions maintain its symbolic value) while its behavioral force has decayed. Residents and practitioners cannot exit the constraint because the stone's authority is embedded in local culture and national disaster management frameworks. Theater ratio (0.81): High. The stone's primary function is now performative: annual ceremonies, school visits, historical plaques, and oral tradition transmission continue, but the stone no longer shapes settlement decisions. The theater has increased over the interval as the stone's behavioral force has decayed and its commemorative function has intensified.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same artifact can be classified differently depending on the observer's structural position. Future coastal residents see pure extraction (Snare) — the stone's loss of behavioral force removes protection without providing alternatives. Disaster risk reduction practitioners see extraction with constrained agency (Snare) — they recognize the problem but cannot enforce solutions. Development interests see coordination (Rope) — the stone marks a boundary that once constrained profitable development, but as behavioral force decays, the constraint appears as coordination without extraction. Heritage institutions see degraded function maintained through performance (Piton) — the stone's commemorative role persists through ritual despite loss of behavioral force. The national disaster management authority sees mixed coordination and extraction (Tangled Rope) — the authority coordinates disaster response while the stone's behavioral force decays, creating asymmetric extraction. The analytical observer risks seeing a natural law (Mountain) — the decay of institutional memory during inter-catastrophe periods is immutable — but the structural data reveals this as a false summit: the stone's degradation benefits identifiable interests (coastal developers) and suppresses alternatives (tsunami risk mitigation capacity).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by the agent's structural position relative to the extraction flow. Future coastal residents are trapped victims with no exit: they bear maximum extraction (d ≈ 1.0). Development interests are beneficiaries with arbitrage options: they experience low or negative extraction (d ≈ 0.2). Disaster risk reduction practitioners are constrained victims: they recognize the problem but cannot exit without institutional backing (d ≈ 0.75). Heritage institutions are constrained beneficiaries: they benefit from the stone's symbolic value but depend on it for funding and legitimacy (d ≈ 0.35). The national disaster management authority is a constrained institutional actor: it coordinates disaster response while the stone's behavioral force decays, creating asymmetric extraction (d ≈ 0.55). The analytical observer is positioned outside the extraction flow but risks naturalizing it (d ≈ 0.5 from the perspective of the false-summit detector).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The Aneyoshi stone directive's mandate (constrain coastal settlement to reduce tsunami risk) has outlived its functional force during the inter-catastrophe period. The stone's behavioral authority has decayed while its commemorative function has intensified. The constraint is now maintained through performative activity (heritage preservation, ritual commemoration) rather than functional necessity (risk mitigation). The mandatrophy is resolved by recognizing that the stone operates in two distinct registers: (1) as a behavioral directive (the behavioral-competence reading, now largely defunct), and (2) as a commemorative artifact (the commemorative-husk reading, now dominant). The constraint story instantiates the second reading, in which the stone's primary function is memorial rather than protective. This reading coexists with the behavioral-competence reading as a live dispute among different stakeholders, but the commemorative-husk reading is the dominant institutional interpretation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_force_decay_mechanism,
    'Is the stone''s loss of behavioral force a natural decay of institutional memory during inter-catastrophe periods, or an engineered suppression by development interests?',
    'Comparative analysis of other disaster-memory stones and directives: do they all decay uniformly during inter-catastrophe periods, or do some maintain behavioral force through active institutional maintenance? Examination of development pressure timelines and stone-maintenance funding patterns.',
    'If natural decay: mountain classification is correct; the constraint is an immutable feature of human memory. If engineered suppression: snare classification is correct; development interests actively benefit from the stone''s degradation. This distinction determines whether the constraint is a natural law or an extractive mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_force_decay_mechanism, empirical, 'Whether behavioral force decay is natural or engineered suppression').

omega_variable(
    reading_kernel_ambiguity,
    'Is the Aneyoshi stone a kernel (a stabilized commitment whose meaning is contested across readings) or a simple artifact whose function has decayed?',
    'Examination of how different communities interpret the stone: does the behavioral-competence reading (the stone still constrains settlement) coexist with this commemorative-husk reading (the stone is now memorial only) as live positions held by different parties? Or has the behavioral reading been definitively superseded?',
    'If kernel: the two readings coexist as live positions; the constraint family includes both behavioral_competence_reading and commemorative_husk_reading as separate stories. If not kernel: this is a single constraint whose function has changed over time, not a reading of a contested commitment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, conceptual, 'Whether the stone is a contested kernel or a simple decayed artifact').

omega_variable(
    inter_catastrophe_period_definition,
    'What temporal threshold defines the ''inter-catastrophe period'' during which behavioral force decays? Is it measured in years, generations, or centuries?',
    'Historical analysis of tsunami return periods in the Aneyoshi region; comparison with other disaster-memory systems and their decay timelines; examination of when the stone''s behavioral force demonstrably ceased to constrain settlement decisions.',
    'If threshold is short (< 50 years): the stone''s decay is rapid and suggests active suppression. If threshold is long (> 200 years): the decay is consistent with natural institutional memory loss. This affects whether the constraint is classified as snare (active extraction) or mountain (natural law).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inter_catastrophe_period_definition, empirical, 'Temporal threshold for inter-catastrophe behavioral force decay').

omega_variable(
    sibling_reading_coexistence,
    'Do the behavioral-competence reading and the commemorative-husk reading coexist as live positions held by different parties, or has one definitively foreclosed the other?',
    'Ethnographic documentation of how different stakeholders (local residents, development interests, heritage institutions, disaster management authorities) interpret the stone''s current function. Do some parties still treat it as a behavioral directive while others treat it as memorial only?',
    'If coexist: the readings are in genuine dispute; the constraint family includes both as separate stories linked by network.affects_constraints. If foreclosed: one reading has won; the other is historical artifact. This determines the reading_relations value (coexists_with vs forecloses).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_coexistence, empirical, 'Whether sibling readings coexist or one forecloses the other').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commemorative_husk_reading, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comhusk_theater_t0_functional, commemorative_husk_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(comhusk_theater_t30_early_performance, commemorative_husk_reading, theater_ratio, 30, 0.45).
narrative_ontology:measurement(comhusk_theater_t60_mid_performance, commemorative_husk_reading, theater_ratio, 60, 0.68).
narrative_ontology:measurement(comhusk_theater_t90_current_memorial, commemorative_husk_reading, theater_ratio, 90, 0.81).

% Extraction over time
narrative_ontology:measurement(comhusk_extractiveness_t0_directive_active, commemorative_husk_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(comhusk_extractiveness_t30_early_decay, commemorative_husk_reading, base_extractiveness, 30, 0.35).
narrative_ontology:measurement(comhusk_extractiveness_t60_mid_decay, commemorative_husk_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement(comhusk_extractiveness_t90_current, commemorative_husk_reading, base_extractiveness, 90, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(comhusk_suppression_t0_natural_compliance, commemorative_husk_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(comhusk_suppression_t30_early_enforcement, commemorative_husk_reading, suppression_requirement, 30, 0.35).
narrative_ontology:measurement(comhusk_suppression_t60_mid_enforcement, commemorative_husk_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement(comhusk_suppression_t90_current_active, commemorative_husk_reading, suppression_requirement, 90, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commemorative_husk_reading, identity_coordination).
narrative_ontology:affects_constraint(commemorative_husk_reading, behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% The Aneyoshi stone directive decomposes into two structurally distinct constraints with different ε values: (1) commemorative_husk_reading (this story) — the stone as memorial artifact with decayed behavioral force, high extractiveness (0.68); (2) behavioral_competence_reading (sibling story) — the stone as active behavioral directive, lower extractiveness. The two readings coexist as live positions held by different stakeholders. The ε-invariance principle requires separate stories because the observable used to evaluate the constraint (does it constrain settlement?) changes the classification outcome. Each story has its own perspectives, beneficiary/victim structure, and measurements. The network link indicates that the behavioral-competence reading is upstream (the stone's original function) and the commemorative-husk reading is downstream (the stone's current function during the inter-catastrophe period).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(commemorative_husk_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
