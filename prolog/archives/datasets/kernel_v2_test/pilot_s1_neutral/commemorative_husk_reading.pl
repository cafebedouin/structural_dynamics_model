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
 *   human_readable: Commemorative Husk Reading: Stone Memorial Directive as Suppression of Coastal Development
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   The Aneyoshi stone directive instantiates a fundamental institutional
 *   tension under this commemorative husk reading: a memorial artifact that
 *   once constrained coastal settlement patterns through behavioral
 *   enforcement has lost that force during inter-catastrophe periods,
 *   enabling profitable coastal development precisely when hazard salience is
 *   lowest and the communities' capacity to resist is weakest. This reading
 *   treats the stone as a contingent institutional arrangement whose
 *   directive authority has atrophied, not as a natural law of memory decay.
 *   The temporal measurement trajectory reveals the mechanism: extractiveness
 *   and suppression both increase during the post-catastrophe recovery period
 *   (t=0–50) as development interests consolidate control over land-use
 *   planning, then stabilize (t=50–75) at elevated levels once the
 *   institutional regime fully establishes itself. Theater ratio rises
 *   steadily (t=0 to t=75), reflecting the progressive transformation of the
 *   stone from an active behavioral constraint to a ritual memorial
 *   maintained for cultural continuity rather than governance function. The
 *   constraint operates at the intersection of three institutional systems:
 *   (1) disaster memory preservation (how communities sustain institutional
 *   knowledge across hazard cycles), (2) land-use governance (how coastal
 *   settlements balance development and safety), and (3) bureaucratic
 *   legitimacy (how institutions maintain symbolic continuity while degrading
 *   material function). This reading argues that the stone's loss of
 *   directive force is not inevitable memory decay but a specific
 *   institutional failure whose beneficiaries are identifiable: coastal
 *   development interests that profit from the weakened constraint.
 *
 * KEY AGENTS:
 *   - Coastal Community: Primary victim (powerless/trapped) — cannot exit settlement; bears tsunami risk as development consolidates in hazard zones
 *   - Coastal Development Interests: Primary beneficiary (institutional/arbitrage) — gain profitable access to high-value beachfront; can exit to other markets if regulatory environment shifts
 *   - Regional Disaster Governance: Secondary actor (organized/constrained) — manages both the stone's maintenance and development approval; experiences the constraint as mixed coordination-extraction
 *   - Memorial Maintenance Bureaucracy: Institutional actor (institutional/constrained) — maintains the stone as cultural artifact; inertia prevents actual behavioral enforcement
 *   - Tsunami Preparedness Epistemic Commons: Victim/witness (institutional/trapped) — institutional knowledge system dependent on continuous behavioral reinforcement; cannot organize separately from the governance structure
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional failure as inevitable psychological law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(commemorative_husk_reading, 0.68).
domain_priors:suppression_score(commemorative_husk_reading, 0.72).
domain_priors:theater_ratio(commemorative_husk_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(commemorative_husk_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(commemorative_husk_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(commemorative_husk_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(commemorative_husk_reading, snare).
narrative_ontology:human_readable(commemorative_husk_reading, "Commemorative Husk Reading: Stone Memorial Directive as Suppression of Coastal Development").
narrative_ontology:topic_domain(commemorative_husk_reading, "disaster_anthropology/institutional_memory/land_use_governance").

domain_priors:requires_active_enforcement(commemorative_husk_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(commemorative_husk_reading, 'c1447f88-3893-4033-bcec-3ebd61c34992').
narrative_ontology:cs_kernel_codification('c1447f88-3893-4033-bcec-3ebd61c34992', fixed_text).
narrative_ontology:cs_authority_grounding('c1447f88-3893-4033-bcec-3ebd61c34992', extraction).
narrative_ontology:cs_interpretation_layer_present('c1447f88-3893-4033-bcec-3ebd61c34992').
narrative_ontology:cs_reading_relation('c1447f88-3893-4033-bcec-3ebd61c34992', commemorative_husk_reading__behavioral_competence_reading, forecloses).
narrative_ontology:cs_axiom('c1447f88-3893-4033-bcec-3ebd61c34992', foundational, directive_force_loss_is_institutional_failure).
narrative_ontology:cs_axiom_status(directive_force_loss_is_institutional_failure, holdable).
narrative_ontology:cs_axiom_grounding('c1447f88-3893-4033-bcec-3ebd61c34992', directive_force_loss_is_institutional_failure, empirically_contingent).
narrative_ontology:cs_axiom('c1447f88-3893-4033-bcec-3ebd61c34992', secondary, development_interests_beneficiary_from_memory_loss).
narrative_ontology:cs_axiom_status(development_interests_beneficiary_from_memory_loss, holdable).
narrative_ontology:cs_axiom_grounding('c1447f88-3893-4033-bcec-3ebd61c34992', development_interests_beneficiary_from_memory_loss, empirically_contingent).
narrative_ontology:cs_reference_frame('c1447f88-3893-4033-bcec-3ebd61c34992', inter_catastrophe_memory_preservation_mandate).
narrative_ontology:cs_drift_state('c1447f88-3893-4033-bcec-3ebd61c34992', contemporary, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('c1447f88-3893-4033-bcec-3ebd61c34992', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(commemorative_husk_reading, aneyoshi_stone_directive).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(commemorative_husk_reading, coastal_development_interests).
narrative_ontology:constraint_beneficiary(commemorative_husk_reading, speculative_real_estate_investors).
narrative_ontology:constraint_victim(commemorative_husk_reading, coastal_communities_memory_preservation).
narrative_ontology:constraint_victim(commemorative_husk_reading, tsunami_preparedness_institutional_continuity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(commemorative_husk_reading, coastal_settlement_residents).
narrative_ontology:constraint_victim(commemorative_husk_reading, regional_governance_structure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communities living in coastal zones documented by the stone as hazardous face escalating tsunami risk as development consolidates in high-value beachfront areas. They cannot exit the settlement without abandoning property and social bonds. The stone's loss of directive force during inter-catastrophe periods means the hazard knowledge that would otherwise constrain development is functionally absent from governance decisions. They bear the concentrated risk of a catastrophic event while development interests capture the economic benefits of beachfront expansion.
narrative_ontology:constraint_stakeholder(commemorative_husk_reading, coastal_settlement_residents, payer,
    powerless, generational, trapped, local).

% Real estate investors, fishing industry expansion projects, and coastal infrastructure companies profit from the weakened directive: they can acquire and develop high-value beachfront properties that would be restricted if the stone's hazard guidance actively constrained settlement. They can exit to other markets if coastal regulation tightens, or renegotiate with governance if political conditions shift. The constraint benefits them by suppressing the institutional knowledge that would otherwise limit their development access.
narrative_ontology:constraint_stakeholder(commemorative_husk_reading, coastal_development_interests, beneficiary,
    institutional, immediate, arbitrage, national).

% Disaster governance institutions manage both the stone's preservation (cultural mandate) and coastal land-use planning (economic mandate). They experience pressure from both directions: maintaining the memorial's symbolic importance while enabling development that governance institutions themselves depend on for tax revenue and economic growth. The governance structure is constrained by budget cycles, bureaucratic inertia, and political pressure from development interests. They have agency to re-instantiate the stone's directive in planning decisions but face institutional barriers (competing mandates, resource constraints) to doing so.
narrative_ontology:constraint_stakeholder(commemorative_husk_reading, regional_governance_structure, agenda_setter,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(commemorative_husk_reading, regional_governance_structure, payer).

% The institutional apparatus dedicated to preserving the stone conducts maintenance, seasonal ceremonies, educational outreach, and cultural programming. This work is substantially performative: the stone is kept in good condition, and its history is communicated to tourists and residents. However, the maintenance system does not actively re-instantiate the stone's original behavioral directive (constraining settlement in hazard zones) during inter-catastrophe periods when hazard salience is low. The bureaucracy is trapped by inertia: dismantling the memorial would be politically costly, but maintaining it as symbol while the directive fails is institutionally easier.
narrative_ontology:constraint_stakeholder(commemorative_husk_reading, memorial_maintenance_bureaucracy, agenda_setter,
    institutional, generational, constrained, local).

% The institutional knowledge of inter-catastrophe vulnerability and preparedness (represented here as an abstract entity, not a human agent) is encoded in the stone's existence and is dependent on continuous behavioral reinforcement during low-hazard periods. When that reinforcement fails (as this reading documents), the knowledge system degrades. Specialist communities maintain some of this knowledge (academic disaster studies, hazard mapping), but it becomes disconnected from the communities most vulnerable to the hazard. The knowledge system is trapped because it cannot advocate for its own preservation — only the communities and governance institutions it serves can do that.
narrative_ontology:constraint_stakeholder(commemorative_husk_reading, tsunami_preparedness_knowledge_system, payer,
    institutional, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(commemorative_husk_reading, tsunami_preparedness_knowledge_system).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Managing coastal settlement patterns and economic development in the context of documented inter-catastrophe tsunami hazards. The stone embodies a coordination solution: it marks a hazard boundary and codifies a behavioral rule (settle above this line or be prepared for periodic catastrophic loss). The governance challenge is how to balance legitimate economic use of coastal zones with the known recurrence of extreme hazards.
% TRANSFER_FUNCTION: The constraint transfers economic value (profitable development access) from the coastal community (who bear concentrated tsunami risk) to development interests (who capture beachfront profits). It also transfers institutional knowledge: the memory of inter-catastrophe vulnerability is transferred from the stone's behavioral enforcement to the development sector's capacity to ignore or reframe that hazard knowledge.
% ABSENT_VOICES: Communities in other coastal zones with similar inter-catastrophe hazards (tsunami zones in Indonesia, the Pacific Ring of Fire, Bangladesh) whose institutional memory systems may operate differently. International disaster governance frameworks that might advocate for stronger inter-catastrophe memory preservation (UN disaster risk reduction, insurance industry risk management). Future catastrophe victims (not yet born) whose safety depends on whether the stone's directive is re-instantiated. These voices are structurally excluded from governance decisions made during inter-catastrophe periods of low hazard salience.
% DISAPPEARANCE_RATIONALE: If the Aneyoshi stone directive were formally abandoned (the stone removed, the hazard guidance no longer referenced in governance, the memory deliberately erased), coastal development patterns would immediately accelerate — zoning restrictions based on the stone's historical guidance would be removed, beachfront property values would increase, and construction would consolidate in the highest-risk zones. The subsequent catastrophe would be far more lethal because the memory constraint would be gone and populations would be even more concentrated. Conversely, if the stone's directive were re-instantiated (governance enforced the hazard guidance, development was restricted in high-risk zones, the stone's memory was continuously reinforced), settlement patterns would shift inland, development would decentralize, and future catastrophe casualties would be reduced. The constraint's disappearance would reshape the entire coastal economy.
% FOUNDING_PROBLEM: Inter-catastrophe tsunami vulnerability: coastal communities in high-risk zones need to sustain institutional knowledge of hazard recurrence across long periods (decades to centuries) of low-salience quiescence, so that when the next catastrophe occurs, they are prepared and settlement patterns are conservative. The stone was erected as a solution to this: a physical memorial that encodes the hazard knowledge and, through its behavioral enforcement (settlement patterns shaped by its location), continuously reinforces the memory even during periods when no actual hazard is imminent.
% FOUNDING_PROBLEM_CORROBORATION: The corroborating evidence for 'founding problem now dead' comes primarily from development interests and governance structures that treat the stone as heritage/cultural symbol rather than hazard guidance. However, the counterevidence is overwhelming: tsunami recurrence is documented (2004 Indian Ocean, 2011 Japan, 2022 Tonga), inter-catastrophe periods are well-established (300–500 year cycles in some zones), and the original founding problem (sustaining memory across quiescence) is not solved — it is simply abandoned. The 'problem dead' verdict comes from beneficiaries of the abandonment, not from independent analysis. A more accurate assessment: the founding problem persists (inter-catastrophe vulnerability is real), but the institutional commitment to solving it has atrophied.
narrative_ontology:disappearance_verdict(commemorative_husk_reading, world_rearranges).
narrative_ontology:founding_problem_status(commemorative_husk_reading, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COASTAL COMMUNITY — The community that erected the memorial stone faces entrapment: the directive's loss of behavioral force during inter-catastrophe periods means warning mechanisms degrade precisely when coastal expansion is most aggressive (low hazard salience = maximum development pressure). The stone becomes decorative; its mandate to constrain development evaporates. The community is trapped because institutional memory loss is not a barrier they can overcome through exit — they cannot leave their settlement, and reconstructing memory costs resources they do not control.
constraint_indexing:constraint_classification(commemorative_husk_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: REGIONAL DISASTER GOVERNANCE — Constrained by budget cycles and inter-catastrophe bureaucratic atrophy. The governance structure experiences both coordination (managing coastal land use) and extraction (resources devoted to the stone's maintenance as theater rather than to actual preparedness). Exit is constrained because governance institutions are obligated to maintain the memorial even as they undermine its directive force.
constraint_indexing:constraint_classification(commemorative_husk_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DEVELOPMENT INDUSTRY — Primary beneficiary. The loss of the stone's behavioral force during inter-catastrophe periods enables maximum leverage for coastal development projects (beachfront real estate, fishing industry expansion, infrastructure concentration). The development sector experiences the constraint as pure coordination of land-use planning — it solves their problem: remove the memory anchor that would otherwise constrain profitable development. They have arbitrage: they can exit to other markets, renegotiate with governance, or simply wait for memory to decay further.
constraint_indexing:constraint_classification(commemorative_husk_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TSUNAMI PREPAREDNESS EPISTEMIC COMMONS — The institutional knowledge of inter-catastrophe vulnerability is trapped. Unlike a human community, it cannot exit or reorganize. The commons is entrapped by the mechanism itself: the stone's directive authority is architecturally dependent on continuous behavioral enforcement during low-hazard periods. During high-hazard periods, the directive regains force (people remember), but during inter-catastrophe lulls, it degrades completely. This trapped epistemic commons bears the extraction cost: knowledge accumulation becomes concentrated in specialist communities disconnected from the coastal settlement that needs it most.
constraint_indexing:constraint_classification(commemorative_husk_reading, snare,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: MEMORIAL MAINTENANCE BUREAUCRACY — The institutional apparatus that manages the stone (cleaning, preservation, seasonal ceremonies) is substantially performative. The theater ratio (0.58) reflects genuine tension: some maintenance is functionally necessary (stone preservation), but much is ritual performance (annual ceremonies, guided tours, educational framing) that substitutes for the actual behavioral governance the stone once provided. The bureaucracy is trapped by inertia — it maintains the memorial because dismantling it would be politically costly, not because the current arrangement serves its original mandate.
constraint_indexing:constraint_classification(commemorative_husk_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — From a universal civilizational view, institutional memory loss during inter-catastrophe periods is a 'natural' consequence of hazard salience dynamics: human attention naturally attenuates when immediate threat is absent. The stone's directive force cannot resist this psychological law. However, this perspective risks naturalizing what is actually a contingent institutional failure — the stone's mandate was specifically to SUSTAIN behavioral force across the cycle, not merely to exist. The 'law' being invoked is actually the failure of the institutional design, not a true natural law.
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
    constraint_indexing:constraint_classification(commemorative_husk_reading, TypeOther, context(agent_power(organized), _, _, _)),
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
 *   Extractiveness (0.68): High, reflecting that the development interests collect substantial benefit (land access, reduced regulatory constraints) while the coastal community bears concentrated costs (increased tsunami risk). The intermediate value (not maximal 0.95) reflects that this is not a pure predatory extraction — some legitimate coordination exists (managing settlement, balancing development with other land uses), and the extraction mechanism depends on the stone's loss of force, which has a contingent institutional cause rather than a structural inevitability. Suppression (0.72): High. The mechanism operates through institutional memory loss: the stone's directive cannot be enforced if governance actors have forgotten it exists or have deprioritized it in development planning. Suppression is not violent coercion but structural: communities cannot exit, and governance institutions control whether the directive is re-instantiated. Theater ratio (0.58): Moderate-high, increasing over time. Initially (t=0, post-catastrophe), the stone functions as behavioral constraint — people remember, adjust settlement patterns, avoid the most hazardous zones. Over the interval, this functional role progressively hollows: the stone is maintained (cleaning, ceremonies, educational materials) but ceases to constrain actual development decisions. By t=75, the stone is primarily a cultural monument whose maintenance is theater — the ritual continues, but the behavioral force is gone. The increasing trajectory reflects this progressive separation of form from function.
 *
 * PERSPECTIVAL GAP:
 *   This reading emphasizes the gap between the development industry's experience (rope/low extraction) and the coastal community's experience (snare/high extraction). The same institutional reality — the stone's loss of behavioral force — benefits one party and traps another. The gap reveals that the 'natural memory decay' narrative (mountain perspective) is a cover story that obscures institutional choices: governance could maintain the stone's directive force through continued enforcement in planning decisions, could legislate hazard zones based on historical guidance, could embed the stone's mandate in regulatory frameworks. That none of this happens reveals the institutional choice favoring development. The piton perspective complicates the snare: the snare operates through an institutional mechanism (bureaucratic maintenance without behavioral enforcement) that is itself a form of inertia and theater. The constraint is snare + piton: the extraction proceeds through the machinery of memorial maintenance, whose non-functional status enables the extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary structure: coastal development interests (identified agents: real estate investors, fishing industry expansion, infrastructure companies). Victim structure: coastal communities (identified agents: settlement residents, disaster-vulnerable populations) and the epistemic commons of tsunami preparedness (identified agent: institutional knowledge systems dependent on continuous behavioral reinforcement). The development interests' arbitrage exit option (they can enter/exit coastal markets, renegotiate with governance, or simply wait for further memory decay) produces low directionality (d ≈ 0.15–0.25), so their experienced extraction chi is negative or minimal. The coastal community's trapped status (they cannot exit settlement, cannot override governance decisions, cannot unilaterally restore the stone's directive force) produces high directionality (d ≈ 0.85–0.95), so their experienced extraction chi is amplified and maximum. The governance structure's d is intermediate (d ≈ 0.50–0.65): they have some agency (can maintain the directive) but are constrained by political pressure from development interests and by bureaucratic inertia. No directionality overrides are necessary — the structural derivation chain produces the correct d values.
 *
 * MANDATROPHY ANALYSIS:
 *   READING-LEVEL MANDATROPHY: The Aneyoshi stone's original mandate was to preserve institutional memory of inter-catastrophe vulnerability and constrain settlement patterns accordingly. Under this commemorative husk reading, that mandate has been substantially obsoleted by the very institutional systems (land-use governance, development planning) that were supposed to implement it. The stone persists as memorial (cultural/educational function), but its behavioral mandate is dead. The snare classification is diagnostically appropriate: the constraint now functions primarily to extract value (development profits) while suppressing the knowledge that would enable the coastal community to resist (memory of prior catastrophes, institutional directives about hazard zones). The piton perspective (institutional degradation maintaining performative ritual) is compatible with snare at a different analytical level: the snare operates because the piton mechanism (bureaucratic maintenance without behavioral enforcement) enables the extraction. Mandatrophy would be RESOLVED by re-instantiating the stone's behavioral force — either through governance reform (re-establishing the directive in development planning) or through population renewal (catastrophe-driven memory reinstatement). This reading rejects the 'natural law' narrative that treats memory loss as inevitable and thus untouchable. The constraint is mandatrophic precisely because its institutional causes are remediable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    directive_loss_mechanism,
    'Is the stone''s loss of behavioral force a consequence of inherent memory decay (natural law) or of institutional failure to re-instantiate the directive''s authority during inter-catastrophe periods (contingent design)?',
    'Comparative analysis: compare the Aneyoshi stone with other cross-cultural disaster memorials (tsunami stones in other Japanese regions, cyclone markers in Australia/Bangladesh, flood stones in Europe). Do ALL such memorials lose directive force during inter-catastrophe periods, or only those whose supporting institutions atrophy? If decay is universal, it approaches natural law; if institutional support varies the outcome, it is design contingency.',
    'If natural law: the reading correctly identifies an immutable memory-hazard cycle, and intervention would require changing the memorial form entirely (e.g., automated reminders, regulatory embedding). If contingent: the institutional apparatus is culpable for failing to sustain the directive during quiescence, and the snare classification is confirmed — suppression arises from deliberate governance atrophy, not psychological inevitability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(directive_loss_mechanism, empirical, 'Whether behavioral force loss is inherent to memory or contingent to institutional design').

omega_variable(
    kernel_reading_contest,
    'Does this stone instantiate a commemorative memorial whose directive capacity has atrophied (this reading: commemorative_husk_reading), or does it instantiate a technically competent warning system whose behavioral parameters decay predictably and can be re-engineered (sibling reading: behavioral_competence_reading)?',
    'The readings occupy a kernel: what is ''the Aneyoshi stone directive''? This reading treats it as an institutional artifact whose meaning has shifted over time — from active behavioral constraint to decorative memorial. The sibling reading treats it as a technical system whose warning function persists and whose parameters are measurable/recalibrable. The contest is: does the stone''s loss of behavioral force represent institutional failure (snare logic) or system degradation requiring technical renewal (different constraint class)? Resolve by: (1) historical analysis of when the directive''s behavioral force was last actively invoked; (2) examination of whether governance structures have attempted to re-instantiate it; (3) analysis of development patterns: do projects explicitly violate the stone''s directive (active ignoring = snare), or do planners simply not reference it (institutional forgetting = piton)?',
    'If this reading (commemorative husk): the stone is fundamentally extractive — its loss of force enables profitable development while the community bears tsunami risk. The sibling reading (behavioral competence) would classify as scaffold or tangled_rope — a technical system requiring modernization but retaining core function. This is a fundamental contest about the kernel''s *nature*: memorial or warning system? The contest cannot be resolved within a single institutional framework — it divides the parties (development interests prefer the husk reading; coastal community needs the competence reading).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether the stone is a decayed memorial (this reading) or a repairable warning system (sibling reading)').

omega_variable(
    beneficiary_structure_opacity,
    'Who explicitly benefits from the stone''s loss of directive force, and do they actively suppress its re-instantiation, or does suppression arise from passive institutional atrophy?',
    'Documentary analysis: examine development permits, governance meeting minutes, and planning decisions. Are coastal development projects explicitly justified by reference to ''updated hazard models'' (suggesting intentional displacement of the stone''s authority), or are they simply approved without engagement with the stone''s directive (passive institutional death)? Interview governance actors: do they perceive the stone as a constraint they have deliberately weakened, or as a historical artifact whose relevance has faded? Active suppression (meeting minutes debating whether to maintain the directive) = snare confirmed. Passive atrophy (nobody mentions the stone in governance; it exists only in tourism materials) = piton confirmed.',
    'If active suppression: development interests are deliberate extractors, and the snare classification is solid. If passive atrophy: the primary mechanism is institutional degradation (piton), and extraction is secondary — development expands because the constraint disappeared, not because agents conspired to remove it. This distinction affects mandatrophy resolution: active snares require confrontation; passive pitons require institutional resurrection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_structure_opacity, empirical, 'Whether suppression is active (intentional directives removal) or passive (institutional memory loss)').

omega_variable(
    inter_catastrophe_window_variability,
    'How much of the directive''s force loss is attributable to the inter-catastrophe period''s length variability, versus the governance structure''s structural capacity to maintain the directive regardless of hazard salience?',
    'Temporal analysis: compare the stone''s directive authority (measured by development constraints, governance invocation, community narrative frequency) across different inter-catastrophe intervals. Short intervals (decadal) vs. long intervals (century-scale). If directive authority is strictly a function of time-since-last-event, the psychological/memory-decay story is supported. If some governance structures maintain the directive across even long intervals while others lose it quickly, institutional capacity matters more than time. Cross-regional comparison: Aneyoshi stone + comparison sites with different inter-catastrophe lengths and different governance responsiveness.',
    'If time-dependent: the constraint is primarily a natural-law problem (memory decay), and the analytical observer''s mountain perspective has merit. If capacity-dependent: the constraint is an institutional design problem, and snare/piton classifications are more appropriate. The ε value would shift downward if time is the primary driver (less extraction, more immutability), upward if institutional failure drives the decay (more extractiveness, more agency culpability).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inter_catastrophe_window_variability, empirical, 'Whether directive loss is time-dependent or governance-capacity-dependent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(commemorative_husk_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_husk_theater_t0, commemorative_husk_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(comm_husk_theater_t25, commemorative_husk_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(comm_husk_theater_t50, commemorative_husk_reading, theater_ratio, 50, 0.58).
narrative_ontology:measurement(comm_husk_theater_t75, commemorative_husk_reading, theater_ratio, 75, 0.61).

% Extraction over time
narrative_ontology:measurement(comm_husk_extract_t0, commemorative_husk_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(comm_husk_extract_t25, commemorative_husk_reading, base_extractiveness, 25, 0.55).
narrative_ontology:measurement(comm_husk_extract_t50, commemorative_husk_reading, base_extractiveness, 50, 0.72).
narrative_ontology:measurement(comm_husk_extract_t75, commemorative_husk_reading, base_extractiveness, 75, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(comm_husk_supp_t0, commemorative_husk_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(comm_husk_supp_t25, commemorative_husk_reading, suppression_requirement, 25, 0.65).
narrative_ontology:measurement(comm_husk_supp_t50, commemorative_husk_reading, suppression_requirement, 50, 0.78).
narrative_ontology:measurement(comm_husk_supp_t75, commemorative_husk_reading, suppression_requirement, 75, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(commemorative_husk_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(commemorative_husk_reading, behavioral_competence_reading).

% DUAL FORMULATION NOTE:
% The Aneyoshi stone instantiates a contested kernel (aneyoshi_stone_directive) with two structurally distinct readings: commemorative_husk_reading (this file) treats the stone as a degraded memorial whose behavioral mandate has failed; behavioral_competence_reading treats it as a technical warning system whose function persists. The two readings have different epsilon values, different beneficiary structures, and different classification outcomes. They are not alternative measurements of the same constraint but two distinct constraints sharing a kernel. The commemorative_husk_reading emphasizes institutional failure and extraction; the behavioral_competence_reading emphasizes technical degradation and coordination/renewal. Both stories are linked via network.affects_constraints to signal their kernel kinship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
