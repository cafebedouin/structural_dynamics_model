% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__existential_matrix_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__existential_matrix_reading, []).

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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: territorial_sovereignty_legitimacy__existential_matrix_reading
 *   human_readable: Existential-Matrix Reading of Territorial Sovereignty (Zero-Sum Survival Framing)
 *   domain: political/international_relations
 *
 * SUMMARY:
 *   This story instantiates the existential-matrix reading of the
 *   territorial-sovereignty-legitimacy kernel: the claim that legitimacy talk
 *   (covenant, self-determination, international recognition) is
 *   epiphenomenal, and that the real driver of the conflict is each side's
 *   structural need for territorial control as a precondition for collective
 *   survival. Under this reading, compromise frameworks like partition or
 *   land-for-peace are not merely difficult to negotiate but structurally
 *   unstable, because accepting a reduced or shared territorial footprint
 *   means accepting existential vulnerability that neither side's political
 *   leadership can sell to its base without triggering internal
 *   delegitimation. The 1993 Oslo-era theater_ratio spike (0.5) reflects a
 *   period when negotiation machinery ran heavily on performative process
 *   (interim agreements, summits) without altering the underlying existential
 *   calculus either side actually operated on. This is ONE of three linked
 *   readings of the same kernel — see cs_structure and network for the
 *   sibling covenant_continuity_reading and self_determination_reading, which
 *   are separate constraint stories with their own ε and stakeholder
 *   structures, not alternative measurements of this one.
 *
 * KEY AGENTS:
 *   - demographically_or_militarily_dominant_faction
 *   - subordinated_population_under_territorial_control
 *   - displaced_or_stateless_residents
 *   - populations_of_contested_border_zones
 *   - international_mediating_bodies
 *   - diaspora_advocacy_networks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.68).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.79).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__existential_matrix_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__existential_matrix_reading, "Existential-Matrix Reading of Territorial Sovereignty (Zero-Sum Survival Framing)").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__existential_matrix_reading, "political/international_relations").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__existential_matrix_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__existential_matrix_reading, '00e04d53-9ec0-4c31-8918-ae97bde3d19b').
narrative_ontology:cs_kernel_codification('00e04d53-9ec0-4c31-8918-ae97bde3d19b', distributed).
narrative_ontology:cs_authority_grounding('00e04d53-9ec0-4c31-8918-ae97bde3d19b', distributed).
narrative_ontology:cs_reading_relation('00e04d53-9ec0-4c31-8918-ae97bde3d19b', territorial_sovereignty_legitimacy__covenant_continuity_reading, influences).
narrative_ontology:cs_reading_relation('00e04d53-9ec0-4c31-8918-ae97bde3d19b', territorial_sovereignty_legitimacy__self_determination_reading, influences).
narrative_ontology:cs_axiom('00e04d53-9ec0-4c31-8918-ae97bde3d19b', foundational, legitimacy_claims_are_epiphenomenal_to_existential_fear).
narrative_ontology:cs_axiom_status(legitimacy_claims_are_epiphenomenal_to_existential_fear, holdable).
narrative_ontology:cs_axiom_grounding('00e04d53-9ec0-4c31-8918-ae97bde3d19b', legitimacy_claims_are_epiphenomenal_to_existential_fear, empirically_contingent).
narrative_ontology:cs_axiom('00e04d53-9ec0-4c31-8918-ae97bde3d19b', secondary, territorial_compromise_is_structurally_unstable_under_mutual_vulnerability).
narrative_ontology:cs_axiom_status(territorial_compromise_is_structurally_unstable_under_mutual_vulnerability, holdable).
narrative_ontology:cs_axiom_grounding('00e04d53-9ec0-4c31-8918-ae97bde3d19b', territorial_compromise_is_structurally_unstable_under_mutual_vulnerability, instrumental).
narrative_ontology:cs_reference_frame('00e04d53-9ec0-4c31-8918-ae97bde3d19b', pre_state_communal_survival_anxiety).
narrative_ontology:cs_drift_state('00e04d53-9ec0-4c31-8918-ae97bde3d19b', post_oslo_negotiation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('00e04d53-9ec0-4c31-8918-ae97bde3d19b', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, demographically_or_militarily_dominant_faction).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, subordinated_population_under_territorial_control).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, displaced_or_stateless_residents).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, populations_of_contested_border_zones).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, populations_of_contested_border_zones).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls the territory's security apparatus, settlement policy, and border administration at the moment the story is authored. Frames its own hold on territory as an existential necessity rather than a policy choice subject to negotiation, which forecloses compromise frameworks that would require accepting military or demographic vulnerability. Benefits from the existential framing because it converts a contestable claim to control into an unbargainable one.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, demographically_or_militarily_dominant_faction, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__existential_matrix_reading, demographically_or_militarily_dominant_faction, beneficiary).

% Lives under the dominant faction's security and administrative control without equivalent political voice. Movement, resource access, and legal status are constrained by an apparatus justified as necessary for the dominant faction's survival. Has no arbitrage exit — cannot relocate the underlying territorial claim, and emigration means abandoning ancestral land and community networks.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, subordinated_population_under_territorial_control, payer,
    powerless, biographical, trapped, local).

% Were removed from or barred from returning to contested territory in prior rounds of conflict, and remain in refugee or diaspora status. The existential-matrix framing treats their claims as non-negotiable threats to the dominant faction's survival, which forecloses return or restitution as policy options regardless of documentary or legal claims they hold.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, displaced_or_stateless_residents, payer,
    powerless, generational, trapped, regional).

% Live in settlements, mixed municipalities, or buffer areas where both sides assert existential stakes. Receive some material benefit from state or para-state investment tied to holding the territory, but bear the security costs, periodic violence, and legal uncertainty that come from occupying ground both existential narratives claim as indispensable.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, populations_of_contested_border_zones, payer,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__existential_matrix_reading, populations_of_contested_border_zones, beneficiary).

% Propose legal and negotiated frameworks (partition plans, land-for-peace formulas, international administration) premised on the idea that the conflict is juridically resolvable. The existential-matrix reading treats their proposals as structurally irrelevant to the actual driver of the conflict, which marginalizes their voice in practice even when they are formally at the negotiating table.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, international_mediating_bodies, excluded,
    institutional, generational, analytical, global).

% Organize politically and financially in support of one side's territorial claims from outside the region, often amplifying existential-threat narratives to mobilize donations and political pressure. They are not exposed to the daily costs of the arrangement but are structurally excluded from the actual bargaining table where local security and administrative decisions are made.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, diaspora_advocacy_networks, excluded,
    organized, generational, mobile, global).

% Study the structural persistence of the conflict across legal regime changes, tracking whether existential framing, legal framing, or demographic framing best predicts outcomes. They have no stake in the territory but their analysis can shift which framing international actors treat as authoritative.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, conflict_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_sovereignty_legitimacy__existential_matrix_reading, demographically_or_militarily_dominant_faction).
narrative_ontology:fixing_cost_class(territorial_sovereignty_legitimacy__existential_matrix_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The existential-matrix framing coordinates in-group solidarity and mobilizes collective sacrifice (military service, economic burden-sharing, political unity) around a survival narrative, which is a genuine function for group cohesion under perceived threat — regardless of whether the threat level justifies the framing's totalizing zero-sum conclusion.
% TRANSFER_FUNCTION: The framing transfers territorial control, security guarantees, and political voice toward whichever faction currently holds demographic or military dominance, and transfers displacement risk, legal uncertainty, and physical insecurity toward populations without comparable power — while simultaneously transferring negotiating leverage away from international legal frameworks toward facts-on-the-ground.
% ABSENT_VOICES: International mediating bodies and diaspora advocacy networks are formally present in negotiations and public discourse but structurally excluded from the decision loci where the existential-matrix framing is actually enforced (security policy, settlement administration, border control) — the framing's own logic treats their input as irrelevant to what it calls the 'real' driver of the conflict.
% DISAPPEARANCE_RATIONALE: Proponents of this reading hold that if the existential framing were dissolved by both sides simultaneously accepting mutual vulnerability, negotiated frameworks could hold and the conflict's shape would change fundamentally. Skeptics of the reading (including proponents of the sibling covenant and self-determination readings) hold that removing this particular framing would not change underlying power asymmetries or legal disputes, so the world would not meaningfully rearrange — the dispute over which is true is itself part of the conflict.
% FOUNDING_PROBLEM: Two national movements each formed under conditions of prior persecution, displacement, or subjugation and each concluded that lacking sovereign territorial control was an existential vulnerability that had previously produced catastrophic loss of life or political agency.
% FOUNDING_PROBLEM_CORROBORATION: Security-studies scholars outside both national movements attest that perceived existential threat remains a genuine driver of policy on both sides, corroborating that the founding problem is live in a psychological/political sense. Legal scholars and international-law bodies outside the benefiting factions attest that the founding problem, considered as a legal sovereignty dispute, has available juridical resolutions that the existential framing itself blocks from being tested — suggesting the framing's persistence is partly self-reinforcing rather than purely responsive to an unresolved threat.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__existential_matrix_reading, contested).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__existential_matrix_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__existential_matrix_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) and suppression (0.79) are both high and suppression exceeds extraction because the reading's persistence depends less on material rent extraction and more on active enforcement of the zero-sum frame itself — security architecture, settlement policy, and border control that foreclose compromise options from being tested. Theater ratio (0.42) captures the substantial share of diplomatic and political activity (summits, interim agreements, peace-process machinery) that performs negotiation without altering the underlying existential calculus, per this reading's own claim that legal/negotiated settlement is structurally epiphenomenal to the real driver. Accessibility collapse (0.62) is moderate rather than near-total: this reading holds compromise frameworks are structurally unstable, not literally impossible, which leaves partial accessibility. Resistance (0.88) is high because every stakeholder group with less power actively resists the framing's foreclosure of their claims, and even segments within the dominant faction contest the totalizing zero-sum conclusion.
 *
 * PERSPECTIVAL GAP:
 *   From the dominant faction's seat, the arrangement looks like necessary self-defense against an unresolvable existential threat — a mountain, not a choice. From the subordinated and displaced seats, the same arrangement looks like enforced extraction of territorial and political goods dressed in survival language. International mediating bodies see a juridically tractable dispute being treated as intractable by design. The engine computes these divergent per-seat classifications from the structural power/exit data; this story does not adjudicate which seat is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   The dominant faction sits near the beneficiary end: it sets the territorial-security agenda and the existential frame converts its de facto control into an unbargainable claim. The subordinated population and displaced/stateless residents sit near the full-target end: trapped exit options, no leverage over the frame that governs their daily lives, and no arbitrage-grade alternative because the underlying territorial claim cannot be relocated. Populations of contested border zones are genuinely mixed — they draw some benefit from state investment tied to holding ground, while bearing the sharpest physical costs of the framing's zero-sum logic, hence the dual role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (historical persecution producing acute vulnerability without sovereign territorial control) is contested as to whether it remains live in the form the existential framing claims, or whether the framing has outlived a narrower original crisis and now functions to block juridical and negotiated alternatives that could address current conditions. Classifying this as tangled_rope rather than snare preserves the genuine coordination function (in-group solidarity and collective-action mobilization under real perceived threat) while still registering the asymmetric extraction imposed on subordinated and displaced populations — collapsing it to pure extraction would erase the reading's genuine (if contested) security-coordination logic; collapsing it to pure coordination (rope) would erase the documented victim set.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epiphenomenal_vs_constitutive_legitimacy,
    'Is juridical/historical legitimacy argumentation genuinely epiphenomenal to an existential driver, or does the existential framing itself function as a rhetorical device that political actors deploy instrumentally while legitimacy claims do independent causal work?',
    'Comparative case analysis: territorial disputes where existential-threat rhetoric was present but legal/demographic settlement nonetheless proceeded (or failed) independent of the rhetoric''s presence, would help isolate whether existential framing is a genuine independent driver or a post-hoc justification riding on other causal factors (power asymmetry, external patronage, resource competition).',
    'If epiphenomenal framing is confirmed, compromise frameworks should be redesigned around neutralizing existential fear rather than adjudicating legal claims. If the framing is shown to be instrumental cover for power-maximizing behavior, the tangled_rope classification would tilt further toward snare, since the ''coordination'' function would be substantially pretextual.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epiphenomenal_vs_constitutive_legitimacy, conceptual, 'Whether existential-threat framing is the real driver or a strategic overlay on power-based motives.').

omega_variable(
    reading_selection_and_committer_structure,
    'This constraint is one of three declared readings of the territorial_sovereignty_legitimacy kernel (covenant_continuity_reading, self_determination_reading, existential_matrix_reading). Which reading a given international actor, court, or population adopts substantially determines what counts as a legitimate resolution — is there any framework-independent way to adjudicate among these readings, or is reading-selection itself downstream of prior political commitment?',
    'No empirical test resolves this directly; it is a genealogical/conceptual question about whether legitimacy-reading selection precedes or follows political allegiance. Longitudinal survey work tracking whether individuals'' reading-preference changes with exposure to new legal or historical evidence (vs. remaining stable across evidence exposure) would provide indirect evidence.',
    'If reading-selection is shown to be causally downstream of prior political commitment rather than upstream of it, then apparent legitimacy disputes between the readings are better modeled as a proxy contest for pre-existing group loyalty, which would argue for treating all three readings'' legal apparatus as substantially theatrical relative to the actual driver of persistence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_selection_and_committer_structure, conceptual, 'Whether the kernel''s three readings are independently adjudicable or downstream of prior allegiance — the committer structure this story is one instance of.').

omega_variable(
    compromise_instability_mechanism,
    'Is the claimed structural instability of two-state or partition frameworks under this reading driven by genuine, unresolvable mutual-vulnerability calculus, or by specific, potentially reformable institutional features (security guarantee design, third-party enforcement credibility, economic integration incentives) that could be engineered to hold even under an existential frame?',
    'Comparative study of partition and power-sharing arrangements elsewhere (post-conflict federations, demilitarized buffer zones with credible external guarantees) that persisted despite initial existential-threat framing on one or more sides, to test whether institutional design can substitute for frame-change.',
    'If institutional design can stabilize compromise despite persistent existential framing, this reading''s zero-sum conclusion is a contingent prediction rather than a structural necessity, weakening the strongest version of the existential-matrix claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compromise_instability_mechanism, empirical, 'Whether zero-sum instability is inherent to the existential frame or an artifact of specific unaddressed institutional design gaps.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__existential_matrix_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 1948, 0.2).
narrative_ontology:measurement(terr_tr_t1967, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 1967, 0.25).
narrative_ontology:measurement(terr_tr_t1993, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 1993, 0.5).
narrative_ontology:measurement(terr_tr_t2000, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(terr_tr_t2010, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(terr_tr_t2024, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 1948, 0.45).
narrative_ontology:measurement(terr_be_t1967, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 1967, 0.55).
narrative_ontology:measurement(terr_be_t1993, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 1993, 0.5).
narrative_ontology:measurement(terr_be_t2000, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(terr_be_t2010, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(terr_be_t2024, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 1948, 0.55).
narrative_ontology:measurement(terr_su_t1967, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 1967, 0.68).
narrative_ontology:measurement(terr_su_t1993, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 1993, 0.6).
narrative_ontology:measurement(terr_su_t2000, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(terr_su_t2010, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(terr_su_t2024, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 2024, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__existential_matrix_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, self_determination_reading).

% DUAL FORMULATION NOTE:
% This story, covenant_continuity_reading, and self_determination_reading are three separate constraint stories decomposing the natural-language label 'sovereignty legitimacy in this territorial dispute' per the epsilon-invariance principle. Each reading has its own epsilon, its own beneficiary/victim structure, and its own claimed type. This story (existential_matrix_reading) treats legitimacy argumentation as epiphenomenal to existential fear and predicts persistent zero-sum instability regardless of legal settlement; covenant_continuity_reading grounds legitimacy in lineage/recognition and would predict a different beneficiary set (those with continuous-presence and international-recognition claims); self_determination_reading grounds legitimacy in modern self-determination applied to demographic majority and continuous residence, predicting yet another beneficiary set. The three readings are linked via affects_constraints because each reading's political traction changes the resource availability and legitimacy conditions the others operate under — e.g., international adoption of a self-determination framing directly pressures the institutional viability of the existential-matrix reading's zero-sum claim, and vice versa.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
