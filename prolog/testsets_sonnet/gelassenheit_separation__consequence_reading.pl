% ============================================================================
% CONSTRAINT STORY: gelassenheit_separation__consequence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gelassenheit_separation__consequence_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gelassenheit_separation__consequence_reading
 *   human_readable: Ordnung by Consequence: Technology Evaluated by Effect on Visiting, Mutual Aid, and Rootedness
 *   domain: religious/social/technology_governance
 *
 * SUMMARY:
 *   This story instantiates the consequence reading of the Gelassenheit
 *   separation kernel: Ordnung councils evaluate each proposed technology by
 *   a fine-grained, empirically-oriented test — does adopting this device
 *   increase or decrease visiting, mutual aid, and geographic rootedness? —
 *   rather than by whether it resembles worldly artifacts (artifact_reading)
 *   or by whether it creates structural entanglement in worldly systems
 *   regardless of appearance (principle_reading). This produces genuinely low
 *   extractiveness: the rule is calibrated to an observable communal good, is
 *   revisable case-by-case, and is defended by demonstrable retention and
 *   mutual-aid outcomes rather than by symbolic consistency alone. The same
 *   kernel — Gelassenheit as separation from the world — is read three
 *   structurally distinct ways across the sibling stories; this is the
 *   reading with the tightest coupling between rule content and stated
 *   purpose, which is why its ε is markedly lower than a naive 'the Amish
 *   reject technology' framing would suggest.
 *
 * KEY AGENTS:
 *   - ordnung_council: agenda_setter (institutional/identity_locked) — administers the case-by-case consequence test
 *   - settled_church_community: beneficiary (organized/constrained) — the dense social fabric the rule protects
 *   - elderly_and_disabled_members: beneficiary (powerless/trapped) — most dependent on preserved mutual aid
 *   - innovation_minded_younger_members: payer (powerless/identity_locked) — bear convenience costs
 *   - members_with_off_settlement_livelihoods: payer (moderate/constrained) — bear commercial friction costs
 *   - artifact_reading_adherents / principle_reading_adherents: excluded — hold sibling readings, not adjudicating here
 *   - sociological_observers: observer (analytical) — corroborate outcomes from outside the tradition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gelassenheit_separation__consequence_reading, 0.18).
domain_priors:suppression_score(gelassenheit_separation__consequence_reading, 0.32).
domain_priors:theater_ratio(gelassenheit_separation__consequence_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gelassenheit_separation__consequence_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gelassenheit_separation__consequence_reading, rope).
narrative_ontology:human_readable(gelassenheit_separation__consequence_reading, "Ordnung by Consequence: Technology Evaluated by Effect on Visiting, Mutual Aid, and Rootedness").
narrative_ontology:topic_domain(gelassenheit_separation__consequence_reading, "religious/social/technology_governance").

domain_priors:requires_active_enforcement(gelassenheit_separation__consequence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gelassenheit_separation__consequence_reading, '7a929d4c-411c-4aa0-9877-21f825dd04fa').
narrative_ontology:cs_kernel_codification('7a929d4c-411c-4aa0-9877-21f825dd04fa', distributed).
narrative_ontology:cs_authority_grounding('7a929d4c-411c-4aa0-9877-21f825dd04fa', practice).
narrative_ontology:cs_interpretation_layer_present('7a929d4c-411c-4aa0-9877-21f825dd04fa').
narrative_ontology:cs_reading_relation('7a929d4c-411c-4aa0-9877-21f825dd04fa', gelassenheit_separation__artifact_reading, coexists_with).
narrative_ontology:cs_reading_relation('7a929d4c-411c-4aa0-9877-21f825dd04fa', gelassenheit_separation__principle_reading, coexists_with).
narrative_ontology:cs_axiom('7a929d4c-411c-4aa0-9877-21f825dd04fa', foundational, visiting_and_mutual_aid_are_the_measure_of_separation).
narrative_ontology:cs_axiom_status(visiting_and_mutual_aid_are_the_measure_of_separation, holdable).
narrative_ontology:cs_axiom_grounding('7a929d4c-411c-4aa0-9877-21f825dd04fa', visiting_and_mutual_aid_are_the_measure_of_separation, instrumental).
narrative_ontology:cs_axiom('7a929d4c-411c-4aa0-9877-21f825dd04fa', secondary, technology_permissibility_is_contextually_revisable_not_categorically_fixed).
narrative_ontology:cs_axiom_status(technology_permissibility_is_contextually_revisable_not_categorically_fixed, holdable).
narrative_ontology:cs_axiom_grounding('7a929d4c-411c-4aa0-9877-21f825dd04fa', technology_permissibility_is_contextually_revisable_not_categorically_fixed, conventional).
narrative_ontology:cs_reference_frame('7a929d4c-411c-4aa0-9877-21f825dd04fa', nineteenth_century_agrarian_gemeinschaft).
narrative_ontology:cs_drift_state('7a929d4c-411c-4aa0-9877-21f825dd04fa', contemporary_digital_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7a929d4c-411c-4aa0-9877-21f825dd04fa', '').
narrative_ontology:cs_kernel_id(gelassenheit_separation__consequence_reading, gelassenheit_separation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, settled_church_community).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, elderly_and_disabled_members).
narrative_ontology:constraint_beneficiary(gelassenheit_separation__consequence_reading, extended_kin_networks).
narrative_ontology:constraint_victim(gelassenheit_separation__consequence_reading, innovation_minded_younger_members).
narrative_ontology:constraint_victim(gelassenheit_separation__consequence_reading, members_with_off_settlement_livelihoods).
narrative_ontology:constraint_vindicates(gelassenheit_separation__consequence_reading, gemeinschaft_preservation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The bishop and ministers deliberate case-by-case on each proposed technology, asking not whether it looks worldly but whether adopting it would draw people away from visiting neighbors, weaken barn-raising and harvest cooperation, or make it easier to move away from the settlement. They administer a rule set that is fine-grained and revisable in light of observed consequences, and they bear responsibility for defending distinctions that outsiders find arbitrary (telephone permitted in the barn, forbidden in the house).
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, ordnung_council, agenda_setter,
    institutional, generational, identity_locked, local).

% The married, land-holding core of the community benefits directly from the preserved density of face-to-face visiting, labor-sharing, and physical proximity that the consequence test protects. Their daily lives are structured around the very practices the rule is calibrated to defend, and they experience the ordnung as functional rather than arbitrary.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, settled_church_community, beneficiary,
    organized, generational, constrained, local).

% Depend most heavily on mutual aid and visiting rounds for daily needs and are the most vulnerable if those practices erode. The consequence-based restriction on in-home telephones and personal transportation preserves the exact social infrastructure they rely on for care, making them net beneficiaries even though they have no voice in setting the rule.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, elderly_and_disabled_members, beneficiary,
    powerless, biographical, trapped, local).

% Multi-generational family networks spanning several settlements benefit from rules that keep travel and communication oriented toward visiting rather than substituting for it, preserving the density of kin obligation and reciprocal labor across the wider community.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, extended_kin_networks, beneficiary,
    organized, generational, constrained, regional).

% Bear the cost of case-by-case restrictions that block conveniences (in-home phones, cars, tractors used for road travel) they see as harmless or even helpful to family life. They cannot simply argue the rule is inconsistent from outside the framework — doing so risks being read as worldly-minded — and formal exit means shunning and separation from family, so most simply comply while quietly resenting specific rulings.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, innovation_minded_younger_members, payer,
    powerless, biographical, identity_locked, local).

% Carpenters, furniture makers, and produce sellers who must interact with the surrounding English economy find the consequence test applied unevenly to their commercial needs — a business phone in a shop may be permitted while the same phone in the home is not, forcing complicated workarounds (phone shanties, hired drivers) that impose real transaction costs to preserve the household visiting norm.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, members_with_off_settlement_livelihoods, payer,
    moderate, biographical, constrained, regional).

% Members and neighboring congregations who read separation as visible distinction from worldly appearance object that the consequence test's fine-grained, function-based distinctions look inconsistent and permit too much technological entanglement so long as it can be framed as preserving visiting. Their objection is aired in inter-congregational meetings but does not govern this settlement's ordnung.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, artifact_reading_adherents, excluded,
    moderate, generational, constrained, local).

% Members who read separation as avoiding structural entanglement in worldly systems (rather than protecting visiting/mutual aid specifically) would evaluate the same technologies by a different test entirely — grid electricity might be rejected as entanglement regardless of its effect on visiting. This reading is live in other communities but not adjudicating here.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, principle_reading_adherents, excluded,
    moderate, generational, constrained, local).

% Researchers studying Amish and Old Order technology adoption document that the consequence-based communities show markedly higher retention of youth and lower rates of formal schism than artifact-based communities, attributing this to the rule's demonstrable functional payoff rather than its symbolic consistency.
narrative_ontology:constraint_stakeholder(gelassenheit_separation__consequence_reading, sociological_observers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gelassenheit_separation__consequence_reading, diffuse).
narrative_ontology:fixing_cost_class(gelassenheit_separation__consequence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the genuine collective-action problem of maintaining a dense, mutually-dependent social fabric — visiting, barn-raising, eldercare, harvest cooperation — against technologies whose adoption elsewhere has been observed to erode exactly those practices by substituting mediated convenience for face-to-face obligation.
% TRANSFER_FUNCTION: Moves convenience and individual autonomy costs from the collective (which retains cohesion, mutual aid capacity, and elder care infrastructure) onto individual members who want technologies the ordnung restricts, and onto members whose livelihoods require off-settlement commercial contact.
% ABSENT_VOICES: Younger members who would prefer household telephones or personal automobiles rarely challenge specific rulings openly, since doing so risks being read as insufficiently committed to Gelassenheit; artifact-reading and principle-reading adherents in neighboring congregations articulate the objection that this reading is too permissive or too restrictive respectively, but from outside this settlement's governance.
% DISAPPEARANCE_RATIONALE: If the consequence-based ordnung vanished, technology adoption would likely converge toward surrounding rural American norms within a generation — in-home phones, personal vehicles, unrestricted power tools — and observable declines in visiting frequency, labor-sharing rates, and geographic clustering (documented in communities that have loosened equivalent rules) would follow, along with an acceleration of youth out-migration.
% FOUNDING_PROBLEM: Nineteenth and twentieth century technological change (telephone, automobile, electrification, tractors) threatened to substitute individualized, distance-spanning convenience for the face-to-face, geographically concentrated mutual dependence that sustained Anabaptist community life; the founding problem was preventing communal dissolution without freezing all technological change categorically.
% FOUNDING_PROBLEM_CORROBORATION: Sociologists of religion (e.g., studies of Old Order retention rates cited by non-Amish academic researchers) independently corroborate that communities applying consequence-based technology tests show measurably higher youth retention and mutual-aid capacity than comparison communities that either forbade all innovation or adopted freely — corroboration from outside the community's own leadership.
narrative_ontology:disappearance_verdict(gelassenheit_separation__consequence_reading, world_rearranges).
narrative_ontology:founding_problem_status(gelassenheit_separation__consequence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gelassenheit_separation__consequence_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gelassenheit_separation__consequence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gelassenheit_separation__consequence_reading, 0.18, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gelassenheit_separation__consequence_reading_tests).
:- end_tests(gelassenheit_separation__consequence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.18 at interval end) because the rule's costs (foregone convenience, commercial friction) are proportionate to and instrumentally justified by a genuine, observable coordination good (sustained visiting/mutual-aid rates), and because the rule is revisable in light of consequences rather than fixed by symbolic form. Suppression is moderate (0.32) — real social cost attaches to defection (identity_locked exit for core members) but there is no coercive apparatus beyond communal sanction and no exit is physically blocked. Theater ratio is low and rising only slightly (0.08 to 0.12): the great majority of ordnung deliberation time is spent on substantive case analysis, not performative signaling, though some drift toward precedent-following ritual is visible over the century-scale interval.
 *
 * PERSPECTIVAL GAP:
 *   From the ordnung_council's seat, the practice is a functioning coordination mechanism continuously validated by observed consequences. From the payer seats (younger members wanting phones, tradespeople wanting easier commercial contact), the same fine-grained rule can read as arbitrary micromanagement — permitted in the barn, forbidden in the house — even though the council's criterion (effect on visiting) is consistent; the engine's per-seat computation should reflect that payers experience real, if modest, extraction despite the rule's low aggregate ε.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (settled community, elderly/disabled members, extended kin networks) are those whose daily practice IS the visiting and mutual-aid fabric the rule protects — they experience the constraint as low-cost or subsidizing. Payers (innovation-minded younger members, off-settlement livelihood holders) bear the friction of restricted convenience and commercial workaround costs without a correspondingly large share of the protected communal good, since their daily activity is oriented outward. The ordnung_council occupies the agenda_setter seat but is itself identity_locked — its members cannot exit the framework they administer without leaving the faith community entirely, which differentiates them from a captured regulator that personally profits.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem_status is authored 'live' rather than 'dead': the threat that mediated technology substitutes for face-to-face mutual obligation is not a historical artifact solved once and then forgotten — it recurs with each new technology (telephone, then automobile, then internet-capable devices), which is precisely why the consequence test remains a functioning, actively-applied decision procedure rather than an inert legacy rule. This blocks a mandatrophy misreading: the low theater ratio and outside sociological corroboration of retention effects distinguish this from a rule that has outlived its function but persists on inertia (which would show rising theater_ratio and no independent corroboration).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consequence_test_administrability,
    'Can a consequence-based test (effect on visiting/mutual aid/rootedness) be applied consistently over time, or does its case-by-case nature inevitably drift toward precedent-following ritual that loses contact with the original empirical question?',
    'Longitudinal tracking of ordnung rulings against measured visiting-frequency and mutual-aid-participation data; rising divergence between rulings and outcomes would indicate drift toward theater.',
    'If the test has drifted toward precedent without re-testing consequences, the currently-low theater_ratio understates actual ritualization and the constraint is closer to a piton than the authored metrics suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consequence_test_administrability, empirical, 'Whether the consequence test remains genuinely empirical or has calcified into precedent.').

omega_variable(
    sibling_reading_boundary_location,
    'Where exactly does the consequence reading''s jurisdiction end and the principle or artifact readings begin, within a single tradition that contains congregations applying all three?',
    'Comparative ethnography across Old Order affiliations documenting which congregations explicitly invoke consequence-based versus artifact-based versus principle-based reasoning in their ordnung deliberations, and whether congregations shift between readings over time.',
    'If congregations fluidly move between readings depending on the specific technology in question, the three sibling constraints may not be cleanly separable institutions but co-present justificatory registers within one deliberative practice — which would argue for treating them as one constraint with variable justification rather than three distinct kernels-readings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_boundary_location, conceptual, 'Whether the three kernel readings are institutionally distinct or co-present justificatory registers.').

omega_variable(
    youth_retention_causal_attribution,
    'Is higher youth retention in consequence-reading communities caused by the perceived fairness/functionality of the rule itself, or by confounding factors (community size, economic opportunity, family structure) correlated with which reading a community adopts?',
    'Controlled comparison of demographically similar Old Order communities differing primarily in their dominant technology-justification reading.',
    'If retention differences are confounded rather than caused by the reading, the corroboration cited in founding_problem_corroboration is weaker than stated and the ''live'' founding-problem status rests on a less secure empirical footing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(youth_retention_causal_attribution, empirical, 'Whether outcome differences are caused by the reading itself or by confounding community characteristics.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gelassenheit_separation__consequence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gela_tr_t0, gelassenheit_separation__consequence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(gela_tr_t20, gelassenheit_separation__consequence_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement(gela_tr_t40, gelassenheit_separation__consequence_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(gela_tr_t60, gelassenheit_separation__consequence_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(gela_tr_t80, gelassenheit_separation__consequence_reading, theater_ratio, 80, 0.11).
narrative_ontology:measurement(gela_tr_t100, gelassenheit_separation__consequence_reading, theater_ratio, 100, 0.12).

% Extraction over time
narrative_ontology:measurement(gela_be_t0, gelassenheit_separation__consequence_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(gela_be_t20, gelassenheit_separation__consequence_reading, base_extractiveness, 20, 0.12).
narrative_ontology:measurement(gela_be_t40, gelassenheit_separation__consequence_reading, base_extractiveness, 40, 0.14).
narrative_ontology:measurement(gela_be_t60, gelassenheit_separation__consequence_reading, base_extractiveness, 60, 0.15).
narrative_ontology:measurement(gela_be_t80, gelassenheit_separation__consequence_reading, base_extractiveness, 80, 0.17).
narrative_ontology:measurement(gela_be_t100, gelassenheit_separation__consequence_reading, base_extractiveness, 100, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(gela_su_t0, gelassenheit_separation__consequence_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(gela_su_t20, gelassenheit_separation__consequence_reading, suppression_requirement, 20, 0.27).
narrative_ontology:measurement(gela_su_t40, gelassenheit_separation__consequence_reading, suppression_requirement, 40, 0.28).
narrative_ontology:measurement(gela_su_t60, gelassenheit_separation__consequence_reading, suppression_requirement, 60, 0.29).
narrative_ontology:measurement(gela_su_t80, gelassenheit_separation__consequence_reading, suppression_requirement, 80, 0.31).
narrative_ontology:measurement(gela_su_t100, gelassenheit_separation__consequence_reading, suppression_requirement, 100, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gelassenheit_separation__consequence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gelassenheit_separation__consequence_reading, 0.1).
narrative_ontology:affects_constraint(gelassenheit_separation__consequence_reading, gelassenheit_separation__artifact_reading).
narrative_ontology:affects_constraint(gelassenheit_separation__consequence_reading, gelassenheit_separation__principle_reading).

% DUAL FORMULATION NOTE:
% Three constraints share the gelassenheit_separation kernel (a single traditional commitment to separation from 'the world') but instantiate structurally distinct evaluative tests with different ε profiles: consequence_reading (this story, lowest ε, ~0.18, evaluated by effect on visiting/mutual-aid/rootedness), artifact_reading (highest suppression/accessibility_collapse, evaluated by visible resemblance to worldly artifacts regardless of function), and principle_reading (evaluated by functional entanglement in worldly systems regardless of visiting effects). Per the ε-invariance principle these are authored as three separate stories rather than one story with a measurement parameter, linked bidirectionally via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
