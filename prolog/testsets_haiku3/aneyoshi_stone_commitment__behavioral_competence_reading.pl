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
 *   constraint_id: aneyoshi_stone_commitment__behavioral_competence_reading
 *   human_readable: Aneyoshi Stone Commitment: Behavioral Competence Reading (Land-Use Regulation)
 *   domain: disaster_anthropology/commitment_systems/temporal_institutional_analysis
 *
 * SUMMARY:
 *   Aneyoshi is a small coastal settlement in Iwate Prefecture, Japan.
 *   Sometime in the distant past (likely after a major tsunami), residents
 *   erected a stone monument with an inscription directing future generations
 *   not to build below its elevation. The stone's location encodes a land-use
 *   rule: the boundary it marks is the tsunami safe line. For 78 years
 *   spanning the interval 1933–2011, Aneyoshi residents followed this rule
 *   without formal legal enforcement, building all structures above the
 *   stone's level. When the 2011 Tōhoku tsunami struck, Aneyoshi experienced
 *   zero casualties while adjacent communities with lower-elevation buildings
 *   suffered devastating losses. This story instantiates the
 *   behavioral_competence_reading: the stone functions as a live regulatory
 *   mechanism whose constraint on building location remained operationally
 *   effective across nearly eight decades and demonstrably saved lives in
 *   2011. The sibling commemorative_husk_reading treats the stone as a
 *   symbolic artifact whose meaning has decayed to mere memorial function, no
 *   longer constraining actual behavior. This reading affirms the opposite:
 *   the stone is the mechanism.
 *
 * KEY AGENTS:
 *   - Aneyoshi residents collective: multi-generational community transmitting the stone's rule through embodied practice and oral tradition
 *   - Oral tradition bearers (elders, knowledge-keepers): agenda-setters maintaining institutional memory anchored to the artifact
 *   - Future generations (tsunami-protected): beneficiaries of the inherited constraint structure
 *   - Meiji modernization authorities: excluded state actors who dismissed the stone as superstition and promoted lower-elevation development
 *   - Development interests (20th century): excluded commercial actors seeking to maximize coastal land use
 *   - Tsunami 2011 observers (researchers, survivors, authorities): witnesses documenting causal efficacy of the stone's constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(aneyoshi_stone_commitment__behavioral_competence_reading, 0.08).
domain_priors:suppression_score(aneyoshi_stone_commitment__behavioral_competence_reading, 0.02).
domain_priors:theater_ratio(aneyoshi_stone_commitment__behavioral_competence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(aneyoshi_stone_commitment__behavioral_competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(aneyoshi_stone_commitment__behavioral_competence_reading, rope).
narrative_ontology:human_readable(aneyoshi_stone_commitment__behavioral_competence_reading, "Aneyoshi Stone Commitment: Behavioral Competence Reading (Land-Use Regulation)").
narrative_ontology:topic_domain(aneyoshi_stone_commitment__behavioral_competence_reading, "disaster_anthropology/commitment_systems/temporal_institutional_analysis").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(aneyoshi_stone_commitment__behavioral_competence_reading, '6a48e267-acdd-4235-9b19-e093cd3f0b56').
narrative_ontology:cs_kernel_codification('6a48e267-acdd-4235-9b19-e093cd3f0b56', distributed).
narrative_ontology:cs_authority_grounding('6a48e267-acdd-4235-9b19-e093cd3f0b56', practice).
narrative_ontology:cs_interpretation_layer_present('6a48e267-acdd-4235-9b19-e093cd3f0b56').
narrative_ontology:cs_reading_relation('6a48e267-acdd-4235-9b19-e093cd3f0b56', aneyoshi_stone_commitment__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('6a48e267-acdd-4235-9b19-e093cd3f0b56', foundational, stone_constraint_operationally_effective).
narrative_ontology:cs_axiom_status(stone_constraint_operationally_effective, holdable).
narrative_ontology:cs_axiom_grounding('6a48e267-acdd-4235-9b19-e093cd3f0b56', stone_constraint_operationally_effective, empirically_contingent).
narrative_ontology:cs_axiom('6a48e267-acdd-4235-9b19-e093cd3f0b56', secondary, material_artifact_enables_transgenerational_memory).
narrative_ontology:cs_axiom_status(material_artifact_enables_transgenerational_memory, holdable).
narrative_ontology:cs_axiom_grounding('6a48e267-acdd-4235-9b19-e093cd3f0b56', material_artifact_enables_transgenerational_memory, instrumental).
narrative_ontology:cs_reference_frame('6a48e267-acdd-4235-9b19-e093cd3f0b56', tsunami_memory_institutional_persistence).
narrative_ontology:cs_drift_state('6a48e267-acdd-4235-9b19-e093cd3f0b56', contemporary_modernization_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('6a48e267-acdd-4235-9b19-e093cd3f0b56', '').
narrative_ontology:cs_kernel_id(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_residents_collective).
narrative_ontology:constraint_beneficiary(aneyoshi_stone_commitment__behavioral_competence_reading, future_generations_tsunami_protected).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The multi-generational community that maintains the tradition of respecting the stone's boundary. They incur higher building costs by placing structures above the tsunami safe line, but they receive the dominant benefit: protection from catastrophic tsunami loss. They also actively set and enforce the constraint through teaching children the stone's meaning, maintaining the artifact's prominence, and social reinforcement of the rule. Exit would require leaving the community, which is constrained by kinship, property ownership, and embedded identity.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_residents_collective, beneficiary,
    moderate, generational, constrained, local).
narrative_ontology:stakeholder_secondary_role(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_residents_collective, agenda_setter).

% Generations not yet born inherit the safe settlement pattern encoded in the stone's location. They benefit from a coordination mechanism that persists without requiring active enforcement, explicit legal authority, or centralized bureaucracy. They incur the same building-cost premium as current residents but gain the safety benefit. Their exit options are bound by geography, inheritance, and the material reality of inherited property and settlement structure.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, future_generations_tsunami_protected, beneficiary,
    powerless, civilizational, trapped, local).

% Community elders and knowledge-keepers who maintain and transmit the stone's meaning and the rule it encodes. They carry the institutional memory anchored to the physical artifact and teach it to younger generations through oral recitation, ceremonial practice, and embodied example. Their role is neither formally salaried nor legally mandated; it is constituted through relational identity within the community (being an elder, being a keeper of tradition). Exit would mean abandoning the defining role that structures their social position and relationships.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, oral_tradition_bearers, agenda_setter,
    moderate, biographical, identity_locked, local).

% Late 19th- and early 20th-century state authorities and officials who promoted coastal development and modernization, often dismissing the stone's constraint as superstitious folk practice. They did not incorporate the stone's rule into formal legal codes and instead promoted lower-elevation development to maximize exploitable land. They are excluded from the constraint's ongoing decision-making because the rule persisted as cultural practice rather than state regulation, and because their power to override local tradition proved limited in practice.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, meiji_modernization_authorities, excluded,
    institutional, biographical, analytical, national).

% Commercial and residential developers who sought to maximize profit from lower-cost coastal land and higher building density. They would have benefited from lower-elevation construction but were excluded from the constraint's enforcement structure because the rule persisted as inherited cultural practice rather than as a contested policy open to negotiation. Their economic interests opposed the constraint but could not override multi-generational community deference to the stone's authority.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, development_interests_20th_century, excluded,
    powerful, biographical, constrained, regional).

% Researchers, disaster-response authorities, journalists, and survivors who documented the 2011 Tōhoku tsunami outcome. They observed and testified that Aneyoshi experienced zero casualties while adjacent communities with lower-elevation buildings suffered devastating losses. They are witnesses to the causal empirical link between 78 years of compliance with the stone's boundary and survival in the 2011 event. Their seat is analytical: they observe and report; they do not directly participate in the constraint's enforcement.
narrative_ontology:constraint_stakeholder(aneyoshi_stone_commitment__behavioral_competence_reading, tsunami_2011_witnesses, observer,
    organized, biographical, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(aneyoshi_stone_commitment__behavioral_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(aneyoshi_stone_commitment__behavioral_competence_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a persistent multi-generational collective-action problem: how to maintain institutional memory of a natural hazard's reach when living memory of the event fades and direct evidence (the tsunami itself) is not continuously salient. The stone localizes the institution—its geographic location IS the encoded rule. Subsequent generations can read the safe boundary off the physical landscape without requiring a centralized enforcer, written legal code, or continuous institutional apparatus.
% TRANSFER_FUNCTION: Transfers behavioral constraint and risk-mitigation obligation from the generation that experiences the original tsunami (high direct salience, clear causal urgency) to future generations who inherit the constraint (low direct salience, attenuated understanding of foundational causal event). The stone and the practice around it move the cost of compliance from 'emergency response to visibly present danger' to 'inherited deference to cultural artifact.' No material goods or rents flow; the transfer is of temporal obligation and risk-assumption.
% ABSENT_VOICES: Commercial developers, state authorities promoting modernization, and private landowners seeking to maximize economic return from scarce coastal property were structurally excluded from the constraint's decision-making apparatus. The constraint persisted as cultural practice anchored to the stone rather than as formal regulation open to policy debate, so those who would have lobbied for relaxation (lower-elevation building, maximized density) never had a formal seat. The Meiji authorities' dismissal of the stone as 'superstition' during early modernization effectively prevented the constraint from being incorporated into written planning law, which itself constituted an exclusionary decision.
% DISAPPEARANCE_RATIONALE: If the stone's authority dissolved, the rule was forgotten, and the cultural practice of deference to the artifact lapsed, Aneyoshi would develop its available coastal land according to profit-maximizing logic. Buildings would move seaward, settlement elevation would drop to match adjacent communities and modern coastal construction norms. The empirical outcome would be directly comparable to the communities surrounding Aneyoshi in 2011: the Tōhoku tsunami struck adjacent areas where buildings were at lower elevation and caused catastrophic casualties. If the stone's constraint vanished, Aneyoshi would assume that same risk profile.
% FOUNDING_PROBLEM: A major tsunami occurred in Aneyoshi's history (the exact date is lost, but the catastrophic event was the generative memory). The generation that survived it chose to mark the safe boundary—the elevation the tsunami did not exceed—with a physical stone monument and instituted a rule that subsequent generations must not build below this line. The founding problem they solved was: 'How do we prevent future generations, whose living memory of the tsunami will fade, from forgetting the hazard boundary and rebuilding into the danger zone, thereby ensuring catastrophic loss when another tsunami strikes?'
% FOUNDING_PROBLEM_CORROBORATION: The 2011 Tōhoku tsunami provides direct empirical validation that the founding problem remains live and causally operative. Aneyoshi residents and independent tsunami researchers (disaster anthropologists, geomorphologists from the Japanese Geomorphological Union, government disaster-response analysts) confirm that Aneyoshi's zero casualties in 2011 correlate directly with the settlement's elevation above the tsunami safe line marked by the stone. Adjacent communities with lower-elevation buildings experienced massive casualties. This outcome is not self-asserted by the community; it is attested by external observers and by the comparative evidence (Aneyoshi with the stone's constraint vs. comparable communities without such constraints). The founding problem—preventing loss of memory about the tsunami boundary across generational turnover—is demonstrated as live by the fact that the constraint remained effective 78 years after the original event, when living memory of the tsunami had faded entirely.
narrative_ontology:disappearance_verdict(aneyoshi_stone_commitment__behavioral_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(aneyoshi_stone_commitment__behavioral_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(aneyoshi_stone_commitment__behavioral_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is very low (0.08) because the stone's rule creates no transfer of resources or asymmetric burden. Residents who build above the stone incur higher construction costs (steeper terrain, more earth moving, longer access roads) than lower-elevation building would require, but this is a cost borne by the community itself for its own safety—a classic coordination cost, not extraction. No external agent collects rents from compliance. Suppression is negligible (0.02) because the rule persists through cultural transmission and embodied practice, not through coercion or threat. A resident violating the rule would face social ostracism or informal family pressure, but the primary mechanism is inherited deference to the artifact's authority: the stone's location is read as a directive, and deviation from it feels wrong rather than forbidden. Theater is low-moderate (0.15) because the stone's functional role (marking the safe boundary) is real, but some portion of compliance involves ritual reenactment and commemoration (annual ceremonies, teaching children the stone's story) that maintains the cultural-transmission function even as living memory of the original tsunami fades. Accessibility_collapse is very high (0.92) because once you understand the stone's rule (and its foundation in tsunami history), the alternative of building below it becomes nearly inconceivable—the catastrophic outcome is salient and spatially proximate. Resistance is very low (0.05) because the constraint aligns with community safety interests; opposition comes only from absent external actors (developers, modernizers) who never secured a position to enforce alternatives.
 *
 * PERSPECTIVAL GAP:
 *   Meiji-era modernization authorities and contemporary development interests would have perceived the stone as an obstacle to rational (profitable) land use and as evidence of 'superstitious' thinking. From their seat, the constraint appears as irrational tradition blocking economic optimization. From the residents' seat, the stone is rational disaster-risk management encoded in material and cultural form. This perspectival divergence is resolved not by adjudicating reasonableness but by observing outcomes: the 2011 tsunami empirically validates the residents' reading. The commemorative_husk_reading occupies an unstable middle ground: it acknowledges the stone's historical origin but claims its behavioral force has decayed, leaving only ritual and symbolic function. The 2011 outcome undermines this claim—behavioral force demonstrably remained active 78 years after the stone's institution.
 *
 * DIRECTIONALITY LOGIC:
 *   Aneyoshi residents occupy a beneficiary position (d near 0.0–0.2): they incur higher building costs but receive the dominant benefit—protection from tsunami hazard. They are not trapped; they could in principle leave the community, but their exit options are constrained by kinship ties, property ownership, and embedded identity. The future-generations beneficiary seat is powerless with no exit, which would normally indicate high d, but their benefit (zero exposure to the tsunami hazard) is substantial and asymmetrically positive—they inherit a safe settlement pattern. The oral tradition bearers occupy a dual position (agenda_setter + identity_locked): they maintain the rule and transmit it; their power is moderate (they influence community decisions through cultural authority), and their exit is identity_locked because abandoning the tradition would mean abandoning a core aspect of their relational identity within the community. Excluded actors (developers, state authorities) never had the option to participate, so directionality does not apply to them in the constraint-beneficiary frame. From the behavioral_competence_reading (this reading), the stone is a pure coordination mechanism with no extraction component—the constraint redistributes risk but not resources.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing catastrophic loss of institutional memory about tsunami hazard across generational boundaries) remains live and causally operative. There is no mandatrophy. The constraint is not a vestigial remnant of a solved problem; the problem it solves (maintaining safe building location despite attenuated collective memory) is confirmed as live by the 2011 outcome. The theater_ratio is low (0.15) because the functional constraint persists alongside the commemorative ritual—the stone is maintained as both a working boundary marker and a memorial, not maintained primarily for theatrical effect. If this reading (behavioral_competence) is correct, the constraint is a rope (genuine coordination mechanism with no extraction) that should remain active indefinitely, barring radical changes to tsunami risk (e.g., protective seawalls that obviate the rule).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_vs_symbolic_persistence,
    'Is compliance with the stone''s rule maintained primarily through operational deference to a functional safety boundary, or through ritual and commemorative practice that has become decoupled from behavioral constraint?',
    'Comparative ethnographic observation: document whether residents consult the stone''s location in actual building-siting decisions, or whether it functions primarily as a site of ceremonial recognition. Examine planning documents, land-purchase records, and oral histories of construction decisions. Post-2011, track whether new buildings in Aneyoshi are sited relative to the stone''s boundary or according to other factors.',
    'If behavioral, the constraint remains a live rope (coordination mechanism). If symbolic, the reading shifts toward commemorative_husk (theater_ratio rises, claimed_type shifts to piton). This omega is the core distinction between the two readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_vs_symbolic_persistence, empirical, 'Whether the stone''s rule is operationally enforced or ceremonially maintained.').

omega_variable(
    causal_attribution_2011,
    'Did Aneyoshi''s zero casualties in the 2011 tsunami result from buildings being sited above the stone''s boundary, or from other protective factors (distance from epicenter, local topography, breakwater protection, evacuation speed)?',
    'Tsunami-modeling analysis: simulate 2011 inundation patterns for Aneyoshi and adjacent communities under counterfactual scenarios (lower building elevations; alternative settlement patterns). Compare predicted casualty rates under each scenario to observed outcomes. Interview Aneyoshi residents about evacuation routes, building-age distribution, and vulnerability factors.',
    'Strong causal attribution to the stone''s boundary supports behavioral_competence_reading and validates extractiveness near zero (the constraint saved lives, not extracted resources). Weak attribution or attribution to other factors would not invalidate the reading but would weaken the empirical case for operational efficacy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(causal_attribution_2011, empirical, 'The causal role of the stone''s elevation boundary in 2011 survival outcomes.').

omega_variable(
    institutional_memory_substrate,
    'Could the behavioral memory of the tsunami-safe boundary persist without the stone artifact, transmitted through oral tradition and landscape familiarity alone?',
    'Comparative ethnographic study: examine other Japanese coastal communities with oral tsunami traditions but no stone monument. Track how far institutional memory persists without material anchoring (typically 2–3 generations in oral-only traditions). Assess whether Aneyoshi''s 78-year retention is unusual and attributable to the stone''s material persistence.',
    'If the stone is essential to retaining behavioral memory across generational turnover, it is a critical infrastructure component of the constraint and extractiveness remains very low (it is a pure public good for safety). If memory persists equivalently in oral-only communities, the stone''s role is primarily commemorative and theater_ratio rises.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_memory_substrate, empirical, 'Whether the stone artifact is necessary to transmit the behavioral rule across generations.').

omega_variable(
    meiji_modernization_suppression,
    'During the Meiji and early 20th-century modernization period, were there active attempts by state or commercial interests to suppress or override the stone''s constraint, and did community resistance to such pressure constitute an enforcement mechanism?',
    'Historical research: examine prefectural planning records, development permits, and land-use disputes from 1870–1945 in Aneyoshi and neighboring communities. Document whether state authorities or developers proposed lower-elevation construction and whether communities resisted. If resistance occurred, characterize whether it invoked the stone''s authority or other grounds.',
    'If active suppression occurred and the community resisted on the stone''s authority, suppression remains low (the constraint was never enforced through coercion but through inherited cultural authority). If no such conflict occurred, suppression was even lower—the constraint faced no active challenge, merely cultural persistence. Either way, suppression ≈ 0.02 is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(meiji_modernization_suppression, empirical, 'Whether modernization authorities attempted to override the stone''s constraint and what defensive mechanisms were deployed.').

omega_variable(
    reading_contest_irreducibility,
    'Is the contest between behavioral_competence_reading and commemorative_husk_reading empirically resolvable, or does it rest on irreducible framing differences about what constitutes ''behavioral'' versus ''symbolic''?',
    'Conceptual analysis: the readings agree on observable facts (the stone exists, the rule is transmitted, zero casualties occurred in 2011) but may disagree on whether the stone''s role in 2011 outcomes is sufficient to establish ''behavioral competence.'' A behavioral_competence proponent reads the outcome as proof of operational constraint; a commemorative proponent might argue the outcome is overdetermined (the stone would not have prevented the same safe settlement elevation even if the stone were forgotten, because the old building stock happened to be placed safely). The readings may be conceptually incommensurable rather than empirically decidable.',
    'If the readings are incommensurable, the contest is a preference omega (depends on framing), and both cs_structure entries (reading_relations, axioms) should reflect deep value disagreement rather than empirical dispute. If the readings are empirically decidable, the 2011 outcome should determine which is structurally correct.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_irreducibility, conceptual, 'Whether the behavioral vs. commemorative distinction is empirically resolvable or conceptually framed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(aneyoshi_stone_commitment__behavioral_competence_reading, 1933, 2011).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aney_tr_t1933, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1933, 0.1).
narrative_ontology:measurement(aney_tr_t1960, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1960, 0.12).
narrative_ontology:measurement(aney_tr_t1980, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 1980, 0.14).
narrative_ontology:measurement(aney_tr_t2000, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(aney_tr_t2011, aneyoshi_stone_commitment__behavioral_competence_reading, theater_ratio, 2011, 0.15).

% Extraction over time
narrative_ontology:measurement(aney_be_t1933, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1933, 0.08).
narrative_ontology:measurement(aney_be_t1960, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1960, 0.09).
narrative_ontology:measurement(aney_be_t1980, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 1980, 0.08).
narrative_ontology:measurement(aney_be_t2000, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 2000, 0.08).
narrative_ontology:measurement(aney_be_t2011, aneyoshi_stone_commitment__behavioral_competence_reading, base_extractiveness, 2011, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(aney_su_t1933, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 1933, 0.02).
narrative_ontology:measurement(aney_su_t1960, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 1960, 0.02).
narrative_ontology:measurement(aney_su_t1980, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 1980, 0.02).
narrative_ontology:measurement(aney_su_t2000, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 2000, 0.02).
narrative_ontology:measurement(aney_su_t2011, aneyoshi_stone_commitment__behavioral_competence_reading, suppression_requirement, 2011, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(aneyoshi_stone_commitment__behavioral_competence_reading, global_infrastructure).
narrative_ontology:boltzmann_floor_override(aneyoshi_stone_commitment__behavioral_competence_reading, 0.12).
narrative_ontology:affects_constraint(aneyoshi_stone_commitment__behavioral_competence_reading, aneyoshi_stone_commitment__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% The Aneyoshi stone commitment kernel admits two structurally distinct constraint readings. The behavioral_competence_reading (this story) treats the stone as an operationally effective land-use rule whose constraint shaped building-location decisions across 78 years (ε ≈ 0.08, coordination rope). The commemorative_husk_reading (sibling story) treats the stone as a ceremonial/memorial artifact whose behavioral force has decayed (ε would be higher, likely piton). The readings share the same artifact and historical record but diverge on the causal mechanism: is compliance driven by deference to a functional safety boundary, or by ritual maintenance of a tradition that has lost behavioral grip? The 2011 Tōhoku tsunami outcome provides empirical pressure on the contest (zero Aneyoshi casualties vs. adjacent communities' massive losses), but the readings may frame causal attribution differently. These stories are linked by network.affects_constraints and should be compared via the reading_relations and axioms declared in cs_structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
