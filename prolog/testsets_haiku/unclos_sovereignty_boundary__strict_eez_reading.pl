% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__strict_eez_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__strict_eez_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: unclos_sovereignty_boundary__strict_eez_reading
 *   human_readable: UNCLOS EEZ Exclusive Boundaries (Strict Article 57 Reading)
 *   domain: international_law/maritime_governance
 *
 * SUMMARY:
 *   The United Nations Convention on the Law of the Sea (UNCLOS) Article 57
 *   establishes a 200-nautical-mile Exclusive Economic Zone (EEZ) for coastal
 *   states, granting them exclusive resource rights over fisheries, seabed
 *   minerals, and energy resources within that boundary. The
 *   strict_eez_reading interprets this boundary as absolute, uniform, and
 *   binding on all states regardless of UNCLOS ratification, and as fully
 *   superseding historical territorial claims, customary occupancy rights,
 *   and freedom-of-navigation principles. This reading benefits
 *   UNCLOS-ratifying coastal states with exclusive resource control and
 *   benefits the international maritime law authority (UNCLOS bodies, ITLOS)
 *   with interpretive legitimacy. It harms overlapping historical claimants
 *   (China, Vietnam, Philippines in Southeast Asia; Russia, indigenous
 *   peoples in the Arctic; Pacific island nations with competing boundary
 *   interpretations) by subordinating their long-established presence to the
 *   distance rule. It also constrains non-ratifier naval powers (historically
 *   the US, but also Russia in some contexts) by enforcing a boundary they
 *   did not consent to. The strict reading stands in deliberate contrast to
 *   two sibling readings: the historical_rights_reading (which asserts that
 *   centuries of occupation and use create rights that override UNCLOS
 *   distance rules) and the non_ratifier_enforcement_reading (which asserts
 *   that freedom of navigation is a customary law principle binding even on
 *   UNCLOS signatories and independent of the treaty). This constraint story
 *   authors the strict_eez_reading only, as a clean ε-invariant constraint
 *   with its own ε, beneficiary/victim set, and suppression profile. The
 *   other readings are separate constraint stories.
 *
 * KEY AGENTS:
 *   - UNCLOS-ratifying coastal states: benefit from exclusive resource control; set enforcement strategy via treaty bodies
 *   - Overlapping historical claimants (China, Vietnam, Philippines, indigenous Pacific communities): lose access to ancestral fishing grounds and seabed resources; constrained exit
 *   - Non-ratifier naval powers (historically US, Russia in some contexts): must limit freedom-of-navigation operations in claimed EZZ; constrained by enforcement despite non-ratification
 *   - UNCLOS/ITLOS institutional authority: benefits from legitimacy and treaty compliance; agenda-setter via dispute settlement
 *   - Flag-state merchant interests (distant-water fishing): excluded from allocation decisions but structurally affected
 *   - Indigenous and small-scale fishing communities: powerless, identity-locked to ancestral grounds, lose access through boundary enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__strict_eez_reading, 0.68).
domain_priors:suppression_score(unclos_sovereignty_boundary__strict_eez_reading, 0.76).
domain_priors:theater_ratio(unclos_sovereignty_boundary__strict_eez_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 0.76).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__strict_eez_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__strict_eez_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__strict_eez_reading, "UNCLOS EEZ Exclusive Boundaries (Strict Article 57 Reading)").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__strict_eez_reading, "international_law/maritime_governance").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__strict_eez_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__strict_eez_reading, '4a57b6b1-b845-4c2c-b9de-47f48cf77e72').
narrative_ontology:cs_kernel_codification('4a57b6b1-b845-4c2c-b9de-47f48cf77e72', fixed_text).
narrative_ontology:cs_authority_grounding('4a57b6b1-b845-4c2c-b9de-47f48cf77e72', lineage).
narrative_ontology:cs_interpretation_layer_present('4a57b6b1-b845-4c2c-b9de-47f48cf77e72').
narrative_ontology:cs_reading_relation('4a57b6b1-b845-4c2c-b9de-47f48cf77e72', unclos_sovereignty_boundary__historical_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('4a57b6b1-b845-4c2c-b9de-47f48cf77e72', unclos_sovereignty_boundary__non_ratifier_enforcement_reading, influences).
narrative_ontology:cs_axiom('4a57b6b1-b845-4c2c-b9de-47f48cf77e72', foundational, eez_boundary_absolute_distance_rule).
narrative_ontology:cs_axiom_status(eez_boundary_absolute_distance_rule, holdable).
narrative_ontology:cs_axiom_grounding('4a57b6b1-b845-4c2c-b9de-47f48cf77e72', eez_boundary_absolute_distance_rule, conventional).
narrative_ontology:cs_axiom('4a57b6b1-b845-4c2c-b9de-47f48cf77e72', foundational, distance_rule_supersedes_historical_claims).
narrative_ontology:cs_axiom_status(distance_rule_supersedes_historical_claims, holdable).
narrative_ontology:cs_axiom_grounding('4a57b6b1-b845-4c2c-b9de-47f48cf77e72', distance_rule_supersedes_historical_claims, deontological).
narrative_ontology:cs_reference_frame('4a57b6b1-b845-4c2c-b9de-47f48cf77e72', coastal_state_eez_exclusivity).
narrative_ontology:cs_drift_state('4a57b6b1-b845-4c2c-b9de-47f48cf77e72', contemporary_overlapping_claims_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4a57b6b1-b845-4c2c-b9de-47f48cf77e72', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, unclos_ratifying_coastal_states).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__strict_eez_reading, international_maritime_law_authority).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, overlapping_historical_claimants).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, non_ratifier_naval_powers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__strict_eez_reading, indigenous_and_small_scale_fishing_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain exclusive resource control (fisheries, seabed minerals, energy) within their 200-nautical-mile EEZ boundaries. They set enforcement strategy through UNCLOS treaty bodies and coastal state naval/administrative action. Can expand their economic zone without competing overlay claims. Exit exists in principle (denunciation of UNCLOS) but carries severe diplomatic and economic costs.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, unclos_ratifying_coastal_states, beneficiary,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__strict_eez_reading, unclos_ratifying_coastal_states, agenda_setter).

% Lose historical fishing rights, territorial occupation claims, and seabed resource access in zones where their long-established presence is superseded by the 200-nautical-mile boundary line. Must either accept the new boundary or contest it through diplomatic or military assertion, both costly. Examples: Vietnam, Philippines, China (as non-exclusive regional claimant), Russia in overlapping arctic zones.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, overlapping_historical_claimants, payer,
    powerful, generational, constrained, global).

% Must limit high-seas freedom-of-navigation operations in zones now claimed as exclusive EEZ by UNCLOS signatories, even though these powers did not ratify the treaty. They retain customary law arguments for unimpeded passage but cannot operate merchant or military vessels in EEZ resource zones without permission. Enforcement via coast guard, naval interception, and sanctions creates ongoing friction.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, non_ratifier_naval_powers, payer,
    powerful, biographical, constrained, global).

% Distant-water fishing fleets and offshore industrial operators have lost access to historically exploited zones. They have no seat at UNCLOS treaty bodies where coastal state EEZ allocations are adjudicated. Their grievance is structural exclusion from the allocation decision, not participation.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, flag_state_merchant_interests, excluded,
    organized, biographical, constrained, global).

% UNCLOS and its dispute settlement bodies (ITLOS) benefit from legitimate authority to interpret and enforce the EEZ regime. Their institutional power grows with treaty compliance and the number of states adopting the boundaries. They adjudicate conflicts between coastal states and outside parties, consolidating maritime law predictability.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, international_maritime_law_authority, beneficiary,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(unclos_sovereignty_boundary__strict_eez_reading, international_maritime_law_authority, agenda_setter).

% In Southeast Asia, the Pacific, and Indian Ocean regions, these communities have customary fishing grounds that now fall within another state's EEZ. They lose access not through legal dispute but through enforcement of a boundary line that supersedes their lived relationship to the water. Their identity is constituted through access to ancestral fishing grounds; the EEZ boundary forecloses that identity practically even where they retain cultural claim.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, indigenous_and_small_scale_fishing_communities, payer,
    powerless, biographical, identity_locked, local).

% View the EEZ regime as enabling coastal state stewardship of marine resources and reduction of unregulated fishing. They also observe that the regime permits coastal states to avoid conservation standards and enable resource depletion within their zones. Their position is mixed: coordination benefit (clear authority for environmental management) plus extraction risk (coastal state capture of conservation policy).
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, environmental_and_conservation_groups, observer,
    organized, generational, analytical, global).

% Island nations, archipelagic states, and continental shelf claimants who interpret UNCLOS differently (e.g., baselines under Article 47 for archipelagic states, continental shelf extensions under Article 76) find their interpretations constrained by the strict 200-nautical-mile ceiling. They are structurally excluded from modifying the core boundary framework even where their geographic position enables different readings.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__strict_eez_reading, land_based_alternative_coastal_claims, excluded,
    moderate, generational, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_sovereignty_boundary__strict_eez_reading, unclos_ratifying_coastal_states).
narrative_ontology:fixing_cost_class(unclos_sovereignty_boundary__strict_eez_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, predictable boundary regime for exclusive maritime resource control. Solves the coordination problem of overlapping territorial claims by replacing contested historical rights with a uniform distance-based rule. Enables coastal states to invest in fisheries management, seabed exploration, and environmental stewardship within a secure, bounded zone.
% TRANSFER_FUNCTION: Transfers exclusive resource rights (fisheries, mineral extraction, energy development, seabed control) from overlapping claimants and flag-state operators to the coastal state whose territory borders the 200-nautical-mile boundary. Also transfers enforcement authority and jurisdictional power from customary-law-based contestation to the UNCLOS treaty framework and its dispute settlement bodies.
% ABSENT_VOICES: Non-ratifier naval powers (notably the United States historically) were excluded from UNCLOS negotiation until late in the process; indigenous fishing communities and small-scale operators have no seat at UNCLOS bodies where EEZ allocations are finalized. Their objections—that historical usage should count, that customary rights predate the treaty, that resource exclusion harms livelihood—are not represented in the treaty framework's design.
% DISAPPEARANCE_RATIONALE: If the strict EEZ reading and its enforcement vanished, overlapping historical claimants would reassert territorial claims, distant-water fishing fleets would resume operations in now-excluded zones, naval powers would challenge boundary enforcement, and maritime governance would revert to negotiated bilateral agreements, customary-law contestation, and deterrent naval presence rather than rules-based allocation.
% FOUNDING_PROBLEM: Post-WWII maritime resource competition created overlapping claims to fisheries, seabed minerals, and strategic waters. The continental shelf convention (1958) and subsequent UNCLOS negotiations (1973–1982) sought to replace contested occupations and customary-law claims with a uniform, objective distance-based rule that would reduce conflict and enable predictable investment in marine resource development.
% FOUNDING_PROBLEM_CORROBORATION: UNCLOS framers and maritime law scholars attest the founding problem was navigation/fishing conflict and boundary uncertainty. However, overlapping historical claimants and non-ratifier powers attest that the founding problem was not solved but relocated—the EEZ boundary CREATES new exclusions rather than resolving old ones. Indigenous Pacific communities and Southeast Asian analysts document that the 'solution' for coastal states is experienced as extraction and loss by communities that had lived relationships to the waters. The corroboration for the founding-problem-solved narrative comes primarily from the beneficiary seats (UNCLOS authorities and ratifying coastal states); the corroboration for the founding-problem-relocated narrative comes from victims and excluded parties.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__strict_eez_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__strict_eez_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__strict_eez_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(unclos_sovereignty_boundary__strict_eez_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__strict_eez_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__strict_eez_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__strict_eez_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__strict_eez_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68 at interval end) because the boundary concentrates exclusive resource rights in coastal state hands, extracting them from overlapping claimants and distant-water operators. The measurement series shows extractiveness rising from 0.45 (early treaty adoption, contested interpretation) to 0.68 (contemporary enforcement with coastal state consolidation and non-ratifier acquiescence). Suppression is highest of all metrics (0.76) because the constraint's persistence depends on active enforcement: coast guard blockade, permit denial, naval deterrence against fishing intrusions, and diplomatic/legal pressure against non-ratifier navigation challenges. Theater ratio is low-to-moderate (0.28) because the boundary enforcement has genuine function (coastal state resource stewardship) even though it also extracts; the theater component comes from framing resource exclusion as 'coordination' when it is experienced as loss by victims. Accessibility collapse is high (0.82) because once the 200-nautical-mile boundary is established and mapped, alternatives (overlapping claims, shared management, open-access zones) become technically and legally inaccessible without full renegotiation or non-ratifier military challenge—few realistic exit pathways exist for victims except through diplomatic claims that the treaty bodies reject.
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary seat (coastal states), this is genuine coordination: a predictable, uniform rule that enables investment in marine resource development and environmental stewardship. From the victim seats (overlapping claimants, non-ratifiers), the same structure operates as enforced extraction: your historical access is erased by a boundary line, and your alternative claims (historical rights, customary navigation, extended continental shelf) are foreclosed by treaty interpretation. The payer and agenda-setter seats (beneficiary coastal states) will compute this as rope—coordination with manageable enforcement costs. The victim seats (overlapping claimants, non-ratifier naval powers) will compute it as snare or tangled_rope—extraction with asymmetric cost. The engine computes per-seat type from directionality; the authored claim of tangled_rope reflects the structural reality that this is neither pure coordination nor pure extraction, but a hybrid: real coordination benefit for coastal state stewardship plus real extraction of alternative claimants' rights, held together by active suppression of competing interpretations.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal states: beneficiary role, institutional power, mobile exit (denunciation of UNCLOS possible but diplomatically/economically costly), generational horizon → directionality near 0.2–0.3 (beneficiaries, low extraction from them). Overlapping claimants: payer role, powerful power (some have naval capacity to contest), constrained exit (relinquishing historical claim is politically costly), generational horizon → directionality near 0.75–0.85 (targets, high extraction from them). Non-ratifier naval powers: payer role, powerful power, constrained exit (accepting the boundary while retaining naval freedom-of-navigation norms is contradictory), biographical horizon (naval policy shifts faster than generational) → directionality near 0.7–0.8 (targets, high extraction in their constrained sphere). Indigenous communities: payer role, powerless power, identity-locked exit (the boundary severed their relationship to ancestral grounds in a way that is not recoverable by mere policy change), biographical horizon → directionality near 0.9+ (targets, maximum extraction and maximum identity lock). The directional asymmetry drives the tangled_rope classification: the same constraint benefits one set asymmetrically and extracts from another, held by active enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was overlapping territorial claims creating fishing conflicts and resource uncertainty post-WWII. The UNCLOS solution was to replace contested boundaries with a uniform 200-nautical-mile rule, enabling coastal states to invest predictably in marine resource development and reducing open-access tragedy-of-the-commons in fisheries. That coordination function remains live and real: coastal states do invest in fisheries management, enforcement capacity, and environmental protection within their bounded EEZ. However, the mandate has accrued a secondary extraction function that was not the founding intent: the boundary now functions as a tool for excluding overlapping historical claimants and constraining non-ratifier navigation rights. The strict reading instantiates this extraction as legitimate treaty interpretation, not as mandate creep. From the beneficiary seat, there is no mandatrophy: the constraint still solves its founding problem (enabling coastal state stewardship and reducing tragedy-of-the-commons). From the victim seat, the founding mandate (reduce conflict) appears dead and replaced with a new function (enforce coastal state monopoly), creating the appearance of mandatrophy. The real story is not mandate obsolescence but mandate capture: the coordination mandate has been successfully co-opted to legitimize extraction, and the suppression of competing readings (historical_rights_reading, non_ratifier_enforcement_reading) ensures the extraction persists. The constraint is tangled_rope, not rope, precisely because this dual function (coordination + extraction) is locked together by suppression and cannot be separated without treaty renegotiation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_rights_vs_distance_rule,
    'Does the strict 200-nautical-mile rule legitimately supersede centuries-long historical usage and occupation in contested maritime zones, or does historical precedent retain equal or superior force?',
    'Test via state practice: examine how UNCLOS bodies and coastal states adjudicate claims where historical rights conflict with the distance rule (e.g., South China Sea disputes, Arctic claims). Observe whether historical claimants'' grievances are heard as substantive challenges or dismissed as foreclosed by the treaty text.',
    'If historical rights retain substantial legitimacy, the strict reading is experienced as extraction by overlapping claimants and may require substantive negotiation or compensation to stabilize. If the distance rule is accepted as supreme, the constraint solidifies as pure coordination. The reading itself instantiates the distance rule as supreme; sibling readings (historical_rights_reading) instantiate the opposite.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_rights_vs_distance_rule, conceptual, 'Whether the strict distance rule legitimately forecloses or merely defers historical rights claims.').

omega_variable(
    customary_law_independence,
    'Is freedom of navigation in contested zones a customary international law principle independent of UNCLOS ratification, or is it subordinate to UNCLOS article 57 for signatory states and their neighbors?',
    'Examine state practice of non-ratifiers (notably the US) and their enforcement challenges. Observe whether non-ratifiers treat the 200-nautical-mile boundary as binding custom or as a treaty-exclusive rule they may ignore. Track enforcement incidents (US naval operations in Chinese EEZ, Russian operations in contested Arctic zones) and their adjudication.',
    'If customary law independence is established, the strict reading has narrower scope: it binds UNCLOS signatories but not non-ratifiers, creating a bifurcated regime. The constraint''s extractiveness for non-ratifiers drops if they retain genuine freedom-of-navigation rights by custom. This is the core structural difference from the non_ratifier_enforcement_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_independence, empirical, 'Whether customary international law provides independent justification for freedom of navigation outside UNCLOS.').

omega_variable(
    indigenous_identity_lock_suppression,
    'Is the suppression of indigenous fishing communities'' access to ancestral grounds structurally enforced (coast guard blockade, permit denial) or internalized (the boundary becomes a psychic reality and communities cease to contest it)?',
    'Post-enforcement trajectory: observe whether communities that lose access reassert claims if enforcement pressure is reduced, or whether multi-generational dispossession has internalized the boundary as inevitable. Track community adaptation narratives: do they preserve historical knowledge and grievance, or do they rationalize the boundary as natural law?',
    'If suppression is structural, removing enforcement would rekindle contests; communities carry grievance forward. If internalized, the constraint persists by theater and identity lock even without active enforcement. The distinction affects whether the constraint remains tangled_rope (active enforcement) or drifts toward piton (theater + internalized acceptance).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigenous_identity_lock_suppression, empirical, 'Degree of structural vs. internalized suppression for powerless indigenous communities.').

omega_variable(
    reading_kernel_identity,
    'This constraint instantiates the strict_eez_reading of the contested unclos_sovereignty_boundary kernel. The sibling readings (historical_rights_reading, non_ratifier_enforcement_reading) instantiate different structural claims about the same UNCLOS text and enforcement practice. What distinguishes this reading from its siblings?',
    'This is a framework ambiguity: the kernel (UNCLOS articles 57–60) can be read as supremely authorizing coastal-state exclusivity (this reading), as subordinate to historical rights and occupancy (historical_rights_reading), or as independently constrained by customary navigation principles outside ratifier jurisdiction (non_ratifier_enforcement_reading). The three readings produce three different constraint families with different victim sets and suppression structures. The resolution is political/jurisprudential: which reading becomes the dominant interpretation within state practice and dispute settlement?',
    'If this reading (strict EEZ) becomes the authoritative interpretation, the historical-rights and non-ratifier readings become empirically foreclosed claims, and the constraint''s extraction profile becomes determinative. If the historical-rights reading gains ground in practice (e.g., through South China Sea arbitration reversals or Arctic consensus-building), the strict reading''s beneficiary set shrinks and its victims may gain footing for compensation or renegotiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_identity, conceptual, 'Kernel reading identity and interpretive dominance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__strict_eez_reading, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(uncl_tr_t0, observed).
narrative_ontology:measurement(uncl_tr_t5, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement_basis(uncl_tr_t5, observed).
narrative_ontology:measurement(uncl_tr_t10, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement_basis(uncl_tr_t10, observed).
narrative_ontology:measurement(uncl_tr_t15, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement_basis(uncl_tr_t15, observed).
narrative_ontology:measurement(uncl_tr_t25, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement_basis(uncl_tr_t25, observed).
narrative_ontology:measurement(uncl_tr_t35, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 35, 0.28).
narrative_ontology:measurement_basis(uncl_tr_t35, observed).
narrative_ontology:measurement(uncl_tr_t45, unclos_sovereignty_boundary__strict_eez_reading, theater_ratio, 45, 0.28).
narrative_ontology:measurement_basis(uncl_tr_t45, observed).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(uncl_be_t0, observed).
narrative_ontology:measurement(uncl_be_t5, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(uncl_be_t5, observed).
narrative_ontology:measurement(uncl_be_t10, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(uncl_be_t10, observed).
narrative_ontology:measurement(uncl_be_t15, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(uncl_be_t15, observed).
narrative_ontology:measurement(uncl_be_t25, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 25, 0.67).
narrative_ontology:measurement_basis(uncl_be_t25, observed).
narrative_ontology:measurement(uncl_be_t35, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(uncl_be_t35, observed).
narrative_ontology:measurement(uncl_be_t45, unclos_sovereignty_boundary__strict_eez_reading, base_extractiveness, 45, 0.68).
narrative_ontology:measurement_basis(uncl_be_t45, observed).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(uncl_su_t0, observed).
narrative_ontology:measurement(uncl_su_t5, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement_basis(uncl_su_t5, observed).
narrative_ontology:measurement(uncl_su_t10, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement_basis(uncl_su_t10, observed).
narrative_ontology:measurement(uncl_su_t15, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 15, 0.72).
narrative_ontology:measurement_basis(uncl_su_t15, observed).
narrative_ontology:measurement(uncl_su_t25, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 25, 0.75).
narrative_ontology:measurement_basis(uncl_su_t25, observed).
narrative_ontology:measurement(uncl_su_t35, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 35, 0.76).
narrative_ontology:measurement_basis(uncl_su_t35, observed).
narrative_ontology:measurement(uncl_su_t45, unclos_sovereignty_boundary__strict_eez_reading, suppression_requirement, 45, 0.76).
narrative_ontology:measurement_basis(uncl_su_t45, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__strict_eez_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(unclos_sovereignty_boundary__strict_eez_reading, 0.18).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary__historical_rights_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, unclos_sovereignty_boundary__non_ratifier_enforcement_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, arctic_extended_continental_shelf_claims).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__strict_eez_reading, south_china_sea_overlapping_eez_disputes).

% DUAL FORMULATION NOTE:
% The unclos_sovereignty_boundary kernel admits three structurally distinct readings instantiated as three constraint families: strict_eez_reading (this file), historical_rights_reading (coordinate but not this file), and non_ratifier_enforcement_reading (coordinate but not this file). The three readings share a single UNCLOS text and enforcement apparatus but instantiate different ε values, beneficiary/victim sets, and suppression profiles. The strict reading's extraction (0.68) flows to coastal states via exclusive resource control; the historical-rights reading's extraction flows away from overlapping claimants who lose the ability to invoke historical occupation; the non_ratifier reading's extraction flows toward non-ratifiers who are constrained by customary-law-independent enforcement. All three are live positions in state practice; none is foreclosed by the others, though they influence each other's operating conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_sovereignty_boundary__strict_eez_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
