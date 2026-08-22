% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__expansive_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__expansive_construction_reading, []).

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
 *   constraint_id: unclos_maritime_sovereignty__expansive_construction_reading
 *   human_readable: Artificial Island Sovereignty Claim (Expansive Construction Reading)
 *   domain: international_law/maritime_governance
 *
 * SUMMARY:
 *   The expansive construction reading of UNCLOS maritime sovereignty claims
 *   that artificial islands built on submerged features or low-tide
 *   elevations generate de facto territorial waters (12nm territorial sea and
 *   extended EEZ) through effective occupation and administrative control.
 *   This reading interprets UNCLOS Article 121 (islands) to apply to
 *   engineered features that acquire human presence and governmental
 *   administration, regardless of natural formation. It vindicates the
 *   doctrine of effective control as applied to modern maritime engineering.
 *   The constraint is CLAIMED as tangled_rope (coordination function:
 *   establishes sovereignty clarity for maritime development; extraction
 *   function: transfers de facto control from international waters to
 *   constructing states) and authored metrics describe substantially
 *   extractive, actively-enforced operation with rising theater
 *   (justifications emphasize strategic/developmental functions while
 *   suppression machinery focuses on blocking competing interpretations). The
 *   reading's core premise is: EFFECTIVE OCCUPATION AND ADMINISTRATIVE
 *   CONTROL of a feature, achieved through physical construction, generates
 *   sovereignty claims under customary international law (refined by UNCLOS)
 *   equivalent to natural islands.
 *
 * KEY AGENTS:
 *   - island_constructing_states: institutional power; generational horizon; mobile exit (can choose whether to continue construction programs); regional scope. Primary beneficiary; sets agenda.
 *   - neighboring_claimant_states: institutional power; generational horizon; constrained exit (diplomatic objection or competitive escalation only); regional scope. Primary victim; displaced by artificial sovereignty claims.
 *   - freedom_of_navigation_states: institutional power; generational horizon; constrained exit (protest, sail-through, litigation, or accept closure); global scope. Secondary victim; international waters narrows.
 *   - international_court_system: institutional power; generational horizon; analytical exit; global scope. Observer; interprets the contested law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__expansive_construction_reading, 0.78).
domain_priors:suppression_score(unclos_maritime_sovereignty__expansive_construction_reading, 0.72).
domain_priors:theater_ratio(unclos_maritime_sovereignty__expansive_construction_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__expansive_construction_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__expansive_construction_reading, "Artificial Island Sovereignty Claim (Expansive Construction Reading)").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__expansive_construction_reading, "international_law/maritime_governance").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__expansive_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__expansive_construction_reading, '36930704-8bb5-40bc-aecc-b645582c6cb7').
narrative_ontology:cs_kernel_codification('36930704-8bb5-40bc-aecc-b645582c6cb7', fixed_text).
narrative_ontology:cs_authority_grounding('36930704-8bb5-40bc-aecc-b645582c6cb7', extraction).
narrative_ontology:cs_interpretation_layer_present('36930704-8bb5-40bc-aecc-b645582c6cb7').
narrative_ontology:cs_reading_relation('36930704-8bb5-40bc-aecc-b645582c6cb7', unclos_maritime_sovereignty__strict_geographic_reading, forecloses).
narrative_ontology:cs_reading_relation('36930704-8bb5-40bc-aecc-b645582c6cb7', unclos_maritime_sovereignty__hybrid_effective_control_reading, coexists_with).
narrative_ontology:cs_axiom('36930704-8bb5-40bc-aecc-b645582c6cb7', foundational, construction_as_effective_occupation).
narrative_ontology:cs_axiom_status(construction_as_effective_occupation, holdable).
narrative_ontology:cs_axiom_grounding('36930704-8bb5-40bc-aecc-b645582c6cb7', construction_as_effective_occupation, empirically_contingent).
narrative_ontology:cs_axiom('36930704-8bb5-40bc-aecc-b645582c6cb7', foundational, administrative_presence_generates_sovereignty).
narrative_ontology:cs_axiom_status(administrative_presence_generates_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('36930704-8bb5-40bc-aecc-b645582c6cb7', administrative_presence_generates_sovereignty, conventional).
narrative_ontology:cs_reference_frame('36930704-8bb5-40bc-aecc-b645582c6cb7', effective_occupation_sovereignty_doctrine).
narrative_ontology:cs_drift_state('36930704-8bb5-40bc-aecc-b645582c6cb7', contemporary_artificial_island_construction_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('36930704-8bb5-40bc-aecc-b645582c6cb7', '2026-06-12T14:37:22Z').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimant_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, commercial_shipping_and_fishing_industries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Execute large-scale maritime engineering projects on submerged features or low-tide elevations (reefs, sandbars, shoals). They claim that physical occupation through dredging, landfill, and infrastructure installation establishes effective control, generating 12nm territorial sea and extended EEZ. They justify the projects as resource development, strategic positioning, and assertion of historical claims. The projects are expensive, politically contentious, and reversible only at enormous cost.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states, agenda_setter,
    institutional, generational, mobile, regional).

% See their own territorial claims and EEZ compressed or displaced by the artificial island's sovereignty assertion. They contest that submerged features cannot generate territorial sea under UNCLOS Article 121(3). Their options are diplomatic objection, filing disputes at international courts, or building their own artificial islands in a competitive escalation cycle. All options are resource-intensive and carry geopolitical risk.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimant_states, payer,
    institutional, generational, constrained, regional).

% Experience narrowing of international waters and high-seas access routes if artificial islands are certified as island-generating territorial sea. They contend that such claims violate the freedom-of-navigation principle and expand effective state control over previously open ocean. Their options are diplomatic protest, sailing through disputed areas asserting rights, funding litigation, or accepting de facto closure.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_states, payer,
    institutional, generational, constrained, global).

% Interprets UNCLOS Articles 60 (artificial structures), 121 (islands), and 121(3) ('rocks which cannot sustain human habitation or economic life of their own') to adjudicate whether artificial islands qualify as generating territorial claims. They do not manufacture facts but must read the law under contested interpretive paradigms.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, international_court_system, observer,
    institutional, generational, analytical, global).

% May inhabit or depend on the natural features or waters that are being engineered. Their voices on prior use, ecological impacts, and displacement are absent from the UNCLOS interpretation process. They are geographically excluded from the table where states negotiate.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, indigenous_and_local_populations, excluded,
    powerless, biographical, trapped, local).

% Navigate shipping lanes and fishing grounds that may shift or become restricted if artificial islands expand territorial waters. They bear increased routing costs, restricted fishing access, and uncertainty about future transits. Their ability to influence the constraint is indirect, through flag state advocacy.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, commercial_shipping_and_fishing_industries, payer,
    organized, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__expansive_construction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Clarifies maritime boundaries and permits state development of submerged features. If the expansive reading holds, states can consolidate control over exclusive economic zones and project power in strategic waterways; the coordination solves the problem of 'who controls submerged features that cannot sustain human populations naturally.'
% TRANSFER_FUNCTION: Transfers de facto territorial sovereignty and exclusive resource rights from the international commons (high seas / disputed waters) to island-constructing states. Neighboring claimants lose claimed territory; freedom-of-navigation states lose guaranteed passage through routes that become territorial waters.
% ABSENT_VOICES: Indigenous and local populations who inhabited or depended on the natural features, ecological researchers who study reef systems and marine habitat loss, non-constructing coastal states without the resources or engineering capacity to build artificial islands, and the shipping and fishing industries whose operational maps change as a side effect.
% DISAPPEARANCE_RATIONALE: If this reading and its enforcement mechanism disappeared (i.e., artificial islands were no longer certified as generating territorial claims), previously constructed islands would face downgrading from island status to artificial structures; territorial waters would contract; resource claims would revert to contested status or international waters; and neighboring states would reorganize their own marine administrative claims. The global maritime governance system would restructure significantly.
% FOUNDING_PROBLEM: UNCLOS (1982) left ambiguous how to treat artificial structures on submerged features: are they islands (Article 121) or artificial structures subject only to safety zones (Article 60)? Island-constructing states, especially in the South China Sea and Indian Ocean, argue that effective occupation through modern engineering is a lawful extension of the historical doctrine of effective control, establishing sovereignty as a natural development of administrative capability.
% FOUNDING_PROBLEM_CORROBORATION: The expansive-reading states (chiefly China, others in regional disputes) attest that the problem is live and their construction demonstrates legitimate effective control. The strict-reading states (USA, UNCLOS originalists, neighboring claimants) attest that UNCLOS Article 121(3) explicitly forecloses this claim—'rocks which cannot sustain human habitation or economic life of their own shall have no territorial sea.' International legal scholarship is split: expansive-reading advocates (citing effective-control doctrine precedent) come from construction-adjacent jurisdictions; strict-reading advocates (citing treaty text and environmental concerns) dominate Western international law institutions. No neutral outside authority has yet issued binding judgment.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__expansive_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__expansive_construction_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__expansive_construction_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unclos_maritime_sovereignty__expansive_construction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__expansive_construction_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__expansive_construction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_maritime_sovereignty__expansive_construction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_maritime_sovereignty__expansive_construction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.62→0.78) because the reading transfers control over vast maritime areas (EEZ, resource rights, strategic positioning) from international commons or disputed waters to individual states at low marginal cost once engineering is complete. The transfer is not justified by coordination benefit (harbor safety, navigation standards) but by constructed facts (islands now exist, are populated/administered, therefore generate claims). Suppression is high (0.72) because the constraint's persistence requires: (1) sustained military/administrative presence on islands to demonstrate effective control, (2) diplomatic and legal blocking of competing interpretations, (3) rejection of neighboring states' alternative readings. Theater is moderate-rising (0.28→0.41): early justifications emphasize scientific research, resource development, and strategic defense; later narrative focuses increasingly on administrative continuity and de facto control rather than coordination. Measurements are authored on a single shared time grid so temporal analysis has complete coverage. Rising extractiveness reflects: initial construction (high cost, legal uncertainty offset by ambition), then consolidation (control becomes effective, extractive benefits accrue, costs shift to competitors). Rising theater reflects: justifications must intensify as the reading's extractive character becomes visible (initially defended as natural development, later defended as necessary sovereignty assertion).
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (neighboring claimants, freedom-of-navigation states, indigenous populations) experience this constraint as enforced territorial seizure by institutional actors with military and diplomatic capacity to suppress challenge. The beneficiary seat (island-constructing state) experiences it as lawful effective control—a doctrine with historical precedent, justified by modern capability, clarifying boundaries that UNCLOS left ambiguous. This gap is structural, not observational: it reflects the asymmetry in who benefits and who bears costs. The engine's per-seat computation will surface this as divergent type verdicts—the constructor may compute as rope (genuine coordination), while the payers compute as snare or tangled_rope (extraction without consent).
 *
 * DIRECTIONALITY LOGIC:
 *   Island-constructing states are beneficiaries (d→0.0): they collect sovereignty, resource rights, and strategic positioning; they have mobile exit (they chose to build and can choose to cease); their power atom is institutional. Neighboring claimant states are targets (d→1.0): they lose claimed territory and must escalate competitively or accept loss; their exit is constrained (confined to diplomacy, litigation, or own construction); power is institutional but subordinate in this constraint's domain (they lack the engineering/military capacity to match). Freedom-of-navigation states are targets (d→0.7+): they lose passage rights and face narrowed international waters; exit is constrained (protest, sail-through risking incident, litigation); they are institutional but not regional beneficiaries of the arrangement. The international court system is analytical (d=0.5): it has no stake in the outcome but must interpret under contested frameworks; exit is defined by institutional role. Directionality overrides are not needed: the structural derivation (beneficiary/victim + exit + power) captures the seats accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem ('UNCLOS ambiguity on artificial features') is live under the expansive reading: constructing states assert ongoing need to develop maritime zones; neighbors assert ongoing need to contest illegitimate claim. BUT the reading's coordination function (boundary clarity) is increasingly offset by its extraction function (territorial seizure). If neighboring states accept the reading and stop challenging, they are accepting permanent loss—not renegotiation of unclear rules but forfeiture to a reinterpreted rule that benefits the constructor. The constraint does NOT resolve the mandatrophy trap—it resolves via acceptance-under-duress, not via genuine coordination. This is structurally tangled_rope: real coordination need (clarify maritime boundaries), real extraction mechanism (constructor unilaterally rewrites the rule to maximize claims), real enforcement (military/diplomatic blockade of competing readings), and victim seats forced into escalation or surrender. The mandate has not outlived its function; the function has been captured by the beneficiary's interpretation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    effective_control_doctrine_scope,
    'Does the customary-law doctrine of effective control (from territorial acquisition via discovery and occupation) apply to submerged features in the maritime domain, or is it confined to terrestrial territory?',
    'Systematic review of arbitral and International Court of Justice precedent on effective control in maritime contexts; comparison of terrestrial vs. maritime acquisition doctrine across legal traditions. The Palau v. Philippines case and ICJ advisory opinions would provide primary evidence.',
    'If effective control applies to maritime features, the expansive reading is strengthened—construction + administration = valid claim. If maritime effective control is inapplicable or requires different elements (natural formation, permanent human habitation), the strict reading prevails—artificial construction is insufficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effective_control_doctrine_scope, conceptual, 'Whether effective-control doctrine has maritime extension.').

omega_variable(
    reading_adoption_and_counter_enforcement,
    'Will neighboring states and freedom-of-navigation states accept the expansive reading de facto, or will counter-enforcement (continued sailing through, rival construction, court challenges) prevent consolidation?',
    'Multi-year observation of: freedom-of-navigation operations (FONOP frequency, incident rates), neighboring state construction projects, international court filings and decisions, third-state diplomatic stances, and actual navigational practice.',
    'If acceptance occurs (neighbors build, FONOP decline, courts defer), the reading consolidates and extraction becomes normalized; suppression becomes institutionalized. If counter-enforcement persists, the reading remains contested and suppression requirements rise (military presence, diplomatic escalation, enforcement machinery intensifies).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_adoption_and_counter_enforcement, empirical, 'Whether the expansive reading consolidates or remains under active challenge.').

omega_variable(
    environmental_and_security_consequences,
    'What are the aggregate ecological impacts (reef damage, species displacement, marine habitat loss) and security escalation risks (militarization of islands, arms buildup, incident likelihood) across the reading''s domain?',
    'Marine ecology surveys pre- and post-construction; incident and accident tracking in disputed waters; military capabilities assessments on constructed islands; climate change modeling (storm surge, saltwater intrusion, island vulnerability).',
    'High ecological damage and escalation risk would support mandatrophy (founding problem solved at enormous hidden cost, constraint persists despite net harm). Low impact would support the reading''s coordination framing (boundaries clarified, development enabled, security actually improves).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(environmental_and_security_consequences, empirical, 'Net consequences of artificial island sovereignty under the expansive reading.').

omega_variable(
    sibling_reading_contest_location,
    'The core dispute between the expansive reading and its siblings (hybrid, strict) is located in the interpretation of UNCLOS Article 121(3): ''Rocks which cannot sustain human habitation or economic life of their own shall have no territorial sea.'' Does construction OVERCOME this qualification (expansive), partially overcome it with time (hybrid), or leave it intact (strict)?',
    'This is a conceptual / interpretive boundary. Resolution depends on: (a) whether the legal community accepts a doctrine of sovereignty maturation (hybrid) or treats Article 121(3) as outcome-determinative (strict), and (b) whether effective occupation + infrastructure is read as ''sustaining human habitation'' (expansive) or as artificial life-support that does not count (strict).',
    'If the strict reading becomes canonical, this constraint is foreclosed as illegitimate. If hybrid becomes dominant, this reading downgrades from tangled_rope to piton (theater rises as the claim persists without legal base). If expansive consolidates, this reading remains tangled_rope—enforced, contested, but increasingly normalized.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_contest_location, conceptual, 'Interpretive placement of the Article 121(3) boundary—where do ''rocks'' end and ''artificially sustained human habitation'' begin?').

omega_variable(
    suppression_internalization_vs_structural,
    'Is the measured suppression (0.72) primarily structural (military/diplomatic capacity to block alternative readings) or internalized (states accepting the reading''s legitimacy after repeated exposure and normalized practice)?',
    'Post-event analysis if counter-enforcement ceases: if neighboring states withdraw challenges because they accept the reading''s legitimacy (not because they lost capacity), suppression is substantially internalized. If they withdraw from exhaustion or fear, suppression is structural. Indicator: statements and internal policy documents from neighboring states about whether the reading is now ''law.''',
    'If internalized, the constraint becomes more stable but also more deceptive (coercion masked by acceptance). If structural, suppression machinery must be continuously maintained (higher enforcement cost, rising theater as justification becomes more elaborate).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_vs_structural, empirical, 'Whether suppression persists as active coercion or becomes normalized acceptance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__expansive_construction_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(uncl_tr_t5, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(uncl_tr_t10, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(uncl_tr_t15, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(uncl_tr_t20, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(uncl_tr_t25, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement(uncl_be_t5, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 5, 0.67).
narrative_ontology:measurement(uncl_be_t10, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 10, 0.72).
narrative_ontology:measurement(uncl_be_t15, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 15, 0.75).
narrative_ontology:measurement(uncl_be_t20, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 20, 0.77).
narrative_ontology:measurement(uncl_be_t25, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 25, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(uncl_su_t5, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(uncl_su_t10, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 10, 0.67).
narrative_ontology:measurement(uncl_su_t15, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 15, 0.7).
narrative_ontology:measurement(uncl_su_t20, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(uncl_su_t25, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__expansive_construction_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(unclos_maritime_sovereignty__expansive_construction_reading, 0.12).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty__strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty__hybrid_effective_control_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, south_china_sea_freedom_of_navigation).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, maritime_resource_competition).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kernel unclos_maritime_sovereignty. The strict_geographic_reading and hybrid_effective_control_reading are sibling readings with different ε values, different beneficiary/victim structures, and different type classifications. They share the same kernel (UNCLOS Article 121 and its interpretation) but instantiate different constraints due to reading-dependent structural differences (what counts as an 'island', what generates sovereignty, what suppression mechanisms are legitimate). Each reading has its own story file with its own metrics, stakeholders, and directionality. The network edges link them as members of the same constraint family—each reading structurally influences the others by competing for interpretive authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_maritime_sovereignty__expansive_construction_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
