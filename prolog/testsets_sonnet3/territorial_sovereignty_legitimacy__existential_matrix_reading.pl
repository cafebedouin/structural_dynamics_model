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
 *   human_readable: Existential-Matrix Reading of Territorial Sovereignty Legitimacy
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This story instantiates the existential-matrix reading of the contested
 *   territorial-sovereignty kernel: legitimacy talk (covenant,
 *   self-determination, international recognition) is treated as
 *   epiphenomenal dressing over an underlying zero-sum survival calculus.
 *   Under this reading, the 1993 Oslo-era dip in theater_ratio reflects a
 *   genuine attempt to substitute negotiated security architecture for
 *   unilateral control, and the subsequent rise reflects the reading's own
 *   prediction: compromise frameworks are structurally unstable because
 *   neither side can accept the vulnerability a shared-sovereignty
 *   arrangement requires, so negotiation performance grows even as
 *   substantive security-sharing recedes. The sibling readings
 *   (covenant_continuity_reading, self_determination_reading) are NOT part of
 *   this file; they are separate constraints with their own ε and stakeholder
 *   structure, linked here only via network and cs_structure fields.
 *
 * KEY AGENTS:
 *   - demographically_or_militarily_dominant_side: primary beneficiary of whatever asymmetry currently obtains (institutional/arbitrage)
 *   - subordinated_population_under_dominant_side: primary target, bears the asymmetry (powerless/trapped)
 *   - displaced_populations_of_both_communities: bears the foreclosure of restitution claims (powerless/trapped)
 *   - civilians_in_contested_border_zones: bears recurring escalation cost (powerless/trapped)
 *   - international_mediators_and_guarantor_states: repeatedly re-author compromise frameworks the reading predicts will fail (institutional/constrained)
 *   - peace_process_analysts: analytical observer across all three kernel readings
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.71).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.79).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__existential_matrix_reading, tangled_rope).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__existential_matrix_reading, "Existential-Matrix Reading of Territorial Sovereignty Legitimacy").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__existential_matrix_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__existential_matrix_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__existential_matrix_reading, '38c12c57-db6c-4f01-aae9-5e37d5a4ba52').
narrative_ontology:cs_kernel_codification('38c12c57-db6c-4f01-aae9-5e37d5a4ba52', distributed).
narrative_ontology:cs_authority_grounding('38c12c57-db6c-4f01-aae9-5e37d5a4ba52', distributed).
narrative_ontology:cs_reading_relation('38c12c57-db6c-4f01-aae9-5e37d5a4ba52', territorial_sovereignty_legitimacy__covenant_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('38c12c57-db6c-4f01-aae9-5e37d5a4ba52', territorial_sovereignty_legitimacy__self_determination_reading, coexists_with).
narrative_ontology:cs_axiom('38c12c57-db6c-4f01-aae9-5e37d5a4ba52', foundational, legitimacy_claims_are_epiphenomenal_to_survival_calculus).
narrative_ontology:cs_axiom_status(legitimacy_claims_are_epiphenomenal_to_survival_calculus, holdable).
narrative_ontology:cs_axiom_grounding('38c12c57-db6c-4f01-aae9-5e37d5a4ba52', legitimacy_claims_are_epiphenomenal_to_survival_calculus, empirically_contingent).
narrative_ontology:cs_axiom('38c12c57-db6c-4f01-aae9-5e37d5a4ba52', secondary, territorial_compromise_is_structurally_unstable_absent_symmetric_vulnerability_acceptance).
narrative_ontology:cs_axiom_status(territorial_compromise_is_structurally_unstable_absent_symmetric_vulnerability_acceptance, holdable).
narrative_ontology:cs_axiom_grounding('38c12c57-db6c-4f01-aae9-5e37d5a4ba52', territorial_compromise_is_structurally_unstable_absent_symmetric_vulnerability_acceptance, instrumental).
narrative_ontology:cs_reference_frame('38c12c57-db6c-4f01-aae9-5e37d5a4ba52', post_1948_unresolved_partition).
narrative_ontology:cs_drift_state('38c12c57-db6c-4f01-aae9-5e37d5a4ba52', post_oslo_framework_collapse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('38c12c57-db6c-4f01-aae9-5e37d5a4ba52', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, demographically_or_militarily_dominant_side).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, subordinated_population_under_dominant_side).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, displaced_populations_of_both_communities).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, civilians_in_contested_border_zones).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, diaspora_and_transnational_advocacy_networks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Whichever party currently holds superior demographic weight, military capacity, or territorial control converts that advantage into de facto sovereignty and frames its own security requirements as existential and non-negotiable. It sets facts on the ground (settlement, annexation, demographic policy, military posture) faster than diplomatic or legal processes can adjudicate them, and treats any proposed compromise that reduces its relative advantage as a threat to survival rather than a negotiable term.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, demographically_or_militarily_dominant_side, beneficiary,
    institutional, civilizational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__existential_matrix_reading, demographically_or_militarily_dominant_side, agenda_setter).

% Lives under the control of whichever side currently holds the advantage, without equivalent territorial sovereignty or security guarantees. Bears movement restriction, administrative subordination, and periodic violence justified by the dominant side's existential framing. Cannot exit the territory and has no independent military or demographic lever to reverse the asymmetry through this same structure.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, subordinated_population_under_dominant_side, payer,
    powerless, generational, trapped, local).

% Populations displaced by prior rounds of conflict (in either direction) whose claims to return or restitution are foreclosed precisely because the existential framing treats any restoration of the other side's territorial position as a survival threat. Their situation is a direct byproduct of the zero-sum logic rather than a side effect of it.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, displaced_populations_of_both_communities, payer,
    powerless, generational, trapped, regional).

% Live in areas where military posture on both sides is calibrated to existential-threat assumptions rather than proportionate defense. Absorb recurring escalation cycles because de-escalation is read by at least one side as conceding the survival margin, regardless of what either side's legal or historical claim would otherwise support.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, civilians_in_contested_border_zones, payer,
    powerless, immediate, trapped, local).

% Propose and underwrite territorial compromise frameworks (partition plans, two-state arrangements, security guarantees) that require both sides to accept some structural vulnerability. Under the existential-matrix reading, these frameworks are structurally undermined at the outset because neither side treats the guarantees as sufficient to offset the loss of unilateral control; mediators repeatedly re-author the same category of proposal without changing the underlying incentive structure.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, international_mediators_and_guarantor_states, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__existential_matrix_reading, international_mediators_and_guarantor_states, observer).

% Advocacy and lobbying networks aligned with each side raise resources, shape international opinion, and reinforce the existential framing domestically and abroad, benefiting from the conflict's persistence as an organizing and fundraising cause, while themselves bearing none of the territorial cost and holding no formal seat in negotiations.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, diaspora_and_transnational_advocacy_networks, beneficiary,
    organized, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(territorial_sovereignty_legitimacy__existential_matrix_reading, diaspora_and_transnational_advocacy_networks, excluded).

% Study why successive compromise frameworks fail to hold, comparing juridical, self-determination, and existential accounts of legitimacy without adjudicating between them, and documenting the repeated collapse of negotiated arrangements when either side perceives a shift in relative territorial or demographic control.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, peace_process_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_sovereignty_legitimacy__existential_matrix_reading, demographically_or_militarily_dominant_side).
narrative_ontology:fixing_cost_class(territorial_sovereignty_legitimacy__existential_matrix_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Under this reading, the arrangement 'coordinates' each side's population around a shared survival narrative that mobilizes collective sacrifice, military service, and resource allocation toward territorial control — a real coordination function for in-group cohesion and defense readiness, decoupled from whether the underlying legitimacy claim is juridically sound.
% TRANSFER_FUNCTION: Moves security, land access, and demographic advantage toward whichever side currently holds dominance, and moves displacement, restricted mobility, and periodic violence onto the subordinated population and civilians in contested zones; also moves donor and diaspora resources into perpetuating mobilization on both sides.
% ABSENT_VOICES: Civilians in border zones and the subordinated population under whichever side currently dominates have no seat in the negotiations that reproduce the existential framing; their assessment of what would actually make them feel secure is not solicited by either side's leadership, which negotiates on their behalf using the survival frame.
% DISAPPEARANCE_RATIONALE: If the existential-matrix framing itself disappeared overnight (i.e., both populations stopped treating territorial control as an all-or-nothing survival precondition), proponents of this reading hold that the underlying security dilemma and identity-survival calculus would reassert itself quickly because it reflects real asymmetric vulnerability, not merely a discourse; proponents of the sibling readings dispute that this framing is load-bearing at all and hold the world would rearrange substantially toward negotiated settlement if it lifted. The verdict is contested within the kernel itself.
% FOUNDING_PROBLEM: Repeated historical experience (for one side, persecution and near-annihilation culminating in genocide; for the other, displacement, statelessness, and military defeat) generated a governing assumption on both sides that anything short of unilateral territorial control leaves the group exposed to existential elimination, not merely political disadvantage.
% FOUNDING_PROBLEM_CORROBORATION: Security-studies scholars and conflict-resolution researchers outside either party's advocacy structure attest that a genuine security dilemma of this kind is common to protracted ethno-territorial conflicts and is not merely rhetorical; however, some of the same researchers, along with negotiators from prior failed frameworks, attest that the existential framing is itself partly maintained by domestic political actors on both sides who benefit electorally from the framing's persistence, making the corroboration itself split between the empirical security-dilemma claim and the political-instrumentalization claim.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__existential_matrix_reading, contested).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__existential_matrix_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__existential_matrix_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.71, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored high (0.71) but not maximal: under this reading the underlying security concern is partly genuine (some fraction of the extraction is coordination cost for real defense needs), so the constraint is not pure snare. Suppression is authored higher (0.79) than extractiveness because the framing's persistence depends on active suppression of alternative security architectures (demilitarized zones, joint sovereignty, international guarantor forces) that would test whether the existential premise is load-bearing — that test is precisely what the framing's proponents on both sides resist allowing to occur. Theater ratio rose from 1993 onward as negotiation processes multiplied without a corresponding increase in substantive risk-sharing, consistent with metric substitution (process performance replacing settlement).
 *
 * PERSPECTIVAL GAP:
 *   From the dominant side's agenda-setting seat, the arrangement reads as coordination — mobilizing collective defense against a genuine existential threat, a real function this reading grants exists in some measure. From the subordinated and displaced populations' seats, the same structure reads as extraction with no coordination benefit returned to them — they bear the costs of a survival calculus conducted entirely by others. This is exactly the seat divergence tangled_rope is built to hold without forcing a single verdict.
 *
 * DIRECTIONALITY LOGIC:
 *   Whichever party currently holds demographic or military dominance is the structural beneficiary (low d) because the existential framing converts its advantage into presumptively legitimate control. Subordinated and displaced populations carry high d because the same framing forecloses their claims and restricts their mobility and restitution options regardless of the legal merits of their case. International mediators sit closer to symmetric/constrained — they invest real institutional resources but cannot capture gains from the arrangement's persistence the way either dominant party can.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (a real historical experience of near-annihilation or mass displacement generating a survival-maximizing security posture) is authored as contested rather than flatly dead: proponents hold the underlying vulnerability is still live; critics (including some negotiators and scholars outside the advocacy structures) hold that the founding trauma is now instrumentalized by domestic political actors who benefit from the framing's persistence independent of whether it still tracks a live threat. This is exactly the founding_problem_status=contested + disappearance_verdict=contested combination the R5 mismatch-consumer is built to flag for further scrutiny rather than resolve by fiat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    existential_fear_genuine_vs_instrumentalized,
    'Is the existential fear driving territorial maximalism on each side a genuine, currently-live security assessment, or is it substantially maintained and amplified by domestic political actors who benefit electorally or organizationally from the framing''s persistence, independent of the underlying threat level?',
    'Longitudinal polling on perceived threat levels cross-referenced with actual military/demographic balance shifts; tracking whether political actors who campaign on existential-threat framing lose support when threat indicators objectively decline; comparative analysis with other protracted conflicts where similar framings did or did not persist past the resolution of the underlying asymmetry.',
    'If substantially instrumentalized, the constraint is better read as tangled_rope shading toward snare for the mobilized populations (real fear is being manufactured/amplified for the benefit of the currently-dominant political actors); if substantially genuine, the coordination function is more load-bearing and the classification sits closer to a hard tangled_rope with less room for compromise-based resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(existential_fear_genuine_vs_instrumentalized, empirical, 'Whether existential fear is a live security assessment or a maintained political instrument.').

omega_variable(
    committer_framing_location,
    'This constraint is one reading (existential_matrix_reading) of the territorial_sovereignty_legitimacy kernel, alongside covenant_continuity_reading and self_determination_reading. The disagreement between readings is located specifically at: whether legitimacy claims are causally prior to (siblings'' view) or epiphenomenal upon (this reading''s view) an underlying survival calculus. A sibling reading would not change the stakeholder set much but would relocate the coordination_function and transfer_function answers entirely onto juridical or demographic-majority grounds rather than existential-security grounds, and would likely lower authored suppression (since juridical/self-determination readings do not treat compromise frameworks as structurally doomed by unresolvable mutual vulnerability).',
    'This is a conceptual/framing question, not empirically resolvable within one reading; it would be addressed by comparative analysis across all three constraint files asking which reading''s predictions about framework durability best match the observed collapse pattern of negotiated settlements (Oslo, Camp David 2000, Annapolis, etc.).',
    'If the existential-matrix account best predicts framework collapse, its extraction and suppression scores are validated as descriptively accurate rather than merely one party''s framing; if a juridical or self-determination account predicts collapse equally well or better, this reading''s exclusivity claim (that legitimacy talk is epiphenomenal) is weakened, though the reading remains a valid, distinct constraint regardless.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(committer_framing_location, conceptual, 'Locates the structural disagreement between this reading and its siblings, and what would change under a sibling reading.').

omega_variable(
    beneficiary_identity_over_time,
    'The declared beneficiary is ''whichever side achieves demographic/military dominance'' — a variable rather than a fixed party. Does the engine''s per-seat computation handle a beneficiary role that is structurally defined as a rotating position rather than a named permanent actor, and does this affect the directionality derivation for either named side across different historical periods within the interval?',
    'Cross-check classification stability by running the same structural data with the beneficiary role attributed to each side in turn at different sub-periods of the 1948-2024 interval (e.g., pre-1967 vs. post-1967 vs. post-1993) and compare computed types.',
    'If the classification is stable regardless of which side is instantiated as the dominant beneficiary, this supports the reading''s core claim that the structure itself (not either side''s specific identity) drives the zero-sum dynamic. If classification is sensitive to which side is named beneficiary, that would suggest the structural data is smuggling in an asymmetric assumption inconsistent with the reading''s stated even-handedness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identity_over_time, conceptual, 'Whether a rotating rather than fixed beneficiary role is handled consistently by the classification.').


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
narrative_ontology:measurement(terr_tr_t1993, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 1993, 0.45).
narrative_ontology:measurement(terr_tr_t2000, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 2000, 0.35).
narrative_ontology:measurement(terr_tr_t2010, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(terr_tr_t2024, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 1948, 0.55).
narrative_ontology:measurement(terr_be_t1967, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 1967, 0.6).
narrative_ontology:measurement(terr_be_t1993, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 1993, 0.58).
narrative_ontology:measurement(terr_be_t2000, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(terr_be_t2010, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(terr_be_t2024, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 2024, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 1948, 0.6).
narrative_ontology:measurement(terr_su_t1967, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 1967, 0.72).
narrative_ontology:measurement(terr_su_t1993, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 1993, 0.68).
narrative_ontology:measurement(terr_su_t2000, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 2000, 0.74).
narrative_ontology:measurement(terr_su_t2010, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 2010, 0.77).
narrative_ontology:measurement(terr_su_t2024, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 2024, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__existential_matrix_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.12).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, self_determination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the territorial_sovereignty_legitimacy kernel, each authored as its own file with its own ε, beneficiary/victim structure, and classification (per the ε-invariance principle — a single natural-language label, 'sovereignty legitimacy in this conflict,' covers three structurally distinct legitimacy claims: divine-covenant-plus-recognition, existential-survival-calculus, and demographic-self-determination). The existential_matrix_reading treats juridical arguments as epiphenomenal, which the covenant_continuity_reading and self_determination_reading both reject at the foundational level (each treats its own juridical/historical argument as load-bearing, not epiphenomenal). All three files link to each other via affects_constraints; none averages ε across the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
