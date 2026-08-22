% ============================================================================
% CONSTRAINT STORY: border_legitimacy__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_legitimacy__sovereignty_reading, []).

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
 *   constraint_id: border_legitimacy__sovereignty_reading
 *   human_readable: Territorial Sovereignty as Ground for Border Exclusion Authority
 *   domain: political_philosophy/migration_law/international_relations
 *
 * SUMMARY:
 *   This story instantiates the sovereignty reading of the contested
 *   border-legitimacy kernel: territorial sovereignty grounds a state's right
 *   to exclude, and border enforcement against economic migrants is read as
 *   the legitimate exercise of that right rather than as a rights violation.
 *   The coordination function is genuine — bounding a political community to
 *   allow self-governance and resource allocation is a real problem
 *   sovereignty solves — but the same authority structure, once vested with
 *   active enforcement machinery, extracts substantially from those it
 *   excludes, particularly non-threatened economic migrants who have no claim
 *   recognized under this reading's own terms. The claimed type is
 *   tangled_rope: coordination for the citizen polity, extraction from
 *   excluded migrants, held in place by active enforcement.
 *
 * KEY AGENTS:
 *   - receiving_state_apparatus: agenda_setter (institutional/analytical) — administers exclusion, collects ordering benefit
 *   - citizen_polity: beneficiary (organized/mobile) — reserved labor market and welfare access
 *   - excluded_economic_migrants: payer (powerless/trapped) — bears exclusion, detention, deportation risk
 *   - mixed_status_families: payer (powerless/trapped) — bears family separation
 *   - stateless_persons: payer (powerless/trapped) — bears the limiting case of universal exclusion
 *   - human_rights_monitors: observer (organized/analytical) — documents but cannot bind
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__sovereignty_reading, 0.68).
domain_priors:suppression_score(border_legitimacy__sovereignty_reading, 0.79).
domain_priors:theater_ratio(border_legitimacy__sovereignty_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__sovereignty_reading, "Territorial Sovereignty as Ground for Border Exclusion Authority").
narrative_ontology:topic_domain(border_legitimacy__sovereignty_reading, "political_philosophy/migration_law/international_relations").

domain_priors:requires_active_enforcement(border_legitimacy__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__sovereignty_reading, '0121319d-6a4c-4b30-a55b-daf6120a4ab5').
narrative_ontology:cs_kernel_codification('0121319d-6a4c-4b30-a55b-daf6120a4ab5', distributed).
narrative_ontology:cs_authority_grounding('0121319d-6a4c-4b30-a55b-daf6120a4ab5', distributed).
narrative_ontology:cs_reading_relation('0121319d-6a4c-4b30-a55b-daf6120a4ab5', border_legitimacy__freedom_of_movement_reading, forecloses).
narrative_ontology:cs_reading_relation('0121319d-6a4c-4b30-a55b-daf6120a4ab5', border_legitimacy__humanitarian_obligation_reading, influences).
narrative_ontology:cs_axiom('0121319d-6a4c-4b30-a55b-daf6120a4ab5', foundational, state_consent_is_necessary_for_membership).
narrative_ontology:cs_axiom_status(state_consent_is_necessary_for_membership, holdable).
narrative_ontology:cs_axiom_grounding('0121319d-6a4c-4b30-a55b-daf6120a4ab5', state_consent_is_necessary_for_membership, conventional).
narrative_ontology:cs_axiom('0121319d-6a4c-4b30-a55b-daf6120a4ab5', foundational, territorial_control_grounds_exclusion_right).
narrative_ontology:cs_axiom_status(territorial_control_grounds_exclusion_right, holdable).
narrative_ontology:cs_axiom_grounding('0121319d-6a4c-4b30-a55b-daf6120a4ab5', territorial_control_grounds_exclusion_right, conventional).
narrative_ontology:cs_reference_frame('0121319d-6a4c-4b30-a55b-daf6120a4ab5', westphalian_territorial_sovereignty).
narrative_ontology:cs_drift_state('0121319d-6a4c-4b30-a55b-daf6120a4ab5', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('0121319d-6a4c-4b30-a55b-daf6120a4ab5', '').
narrative_ontology:cs_kernel_id(border_legitimacy__sovereignty_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, citizen_polity).
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, receiving_state_apparatus).
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, domestic_labor_incumbents).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, excluded_economic_migrants).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, mixed_status_families).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, stateless_persons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, border_enforcement_personnel).
narrative_ontology:constraint_vindicates(border_legitimacy__sovereignty_reading, territorial_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(border_legitimacy__sovereignty_reading, state_consent_basis_of_membership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces admission criteria, operates border control, detention, and removal infrastructure, and justifies exclusion as the exercise of a sovereign prerogative inherent to statehood. Collects the ordering benefit of a controlled population and controlled labor market, and bears the fiscal and diplomatic costs of enforcement, which it treats as the price of self-determination.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, receiving_state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Holds the membership that admission rules protect: access to labor markets, welfare systems, and political voice is reserved to those inside the boundary. Citizens can leave the territory at will and retain membership regardless; the border's exclusionary function operates entirely on others, not on them.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, citizen_polity, beneficiary,
    organized, generational, mobile, national).

% Benefit from reduced labor market competition where exclusion holds, particularly in lower-wage sectors. Their gain is a byproduct of the sovereignty claim rather than its stated justification, but it is real and durable as long as enforcement holds.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, domestic_labor_incumbents, beneficiary,
    moderate, biographical, constrained, national).

% Seek entry for reasons the sovereignty framework classifies as insufficiently compelling to override the state's exclusion right. Face detention, deportation, or indefinite exclusion; have no legal standing to contest the underlying premise that the state may exclude them, only the application of stated criteria. Their labor, safety, and family ties outside the recognized categories carry no independent weight against the sovereign claim.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, excluded_economic_migrants, payer,
    powerless, biographical, trapped, global).

% Contain both members with legal status and members without; the sovereignty framework's enforcement can separate or expel family members regardless of family unity, since territorial membership rather than kinship is the operative category. Exit for the undocumented member typically means family separation, not relief.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, mixed_status_families, payer,
    powerless, biographical, trapped, national).

% Lack a sovereign state whose territorial claim would ground a reciprocal right of return or protection; under a strict sovereignty reading they have no state to assert a countervailing membership claim, so exclusion by every state simultaneously can leave them with no legally recognized place at all.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, stateless_persons, payer,
    powerless, civilizational, trapped, global).

% Carry out apprehension, detention, and removal operations. They administer the sovereignty claim in its most direct form and bear the psychological and sometimes legal costs of enforcement actions, including exposure to allegations of excessive force or rights violations, while having little influence over the policy they enforce.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, border_enforcement_personnel, agenda_setter,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__sovereignty_reading, border_enforcement_personnel, payer).

% Document conditions at borders and in detention, testify before international bodies, and press claims that sovereignty cannot fully override non-refoulement and other obligations. Their analysis is admissible in international fora but not binding on the enforcing state's domestic sovereignty claim.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, human_rights_monitors, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_legitimacy__sovereignty_reading, diffuse).
narrative_ontology:fixing_cost_class(border_legitimacy__sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Territorial sovereignty coordinates a bounded political community: it lets a defined population set entry rules, allocate scarce public goods and labor-market access among members, and maintain a stable basis for collective self-governance without every admission decision being externally contestable.
% TRANSFER_FUNCTION: The arrangement moves protection, labor-market access, and welfare eligibility toward the existing citizen population and away from those seeking entry; it moves enforcement costs and risk onto border personnel and moves the cost of exclusion — displacement, family separation, statelessness, and risk of harm at the border — onto excluded migrants and their families.
% ABSENT_VOICES: Excluded migrants have no standing within the sovereign state's own legal order to contest the premise that the state may exclude them at all — they can only contest procedural application of criteria they did not help set. Sending-state governments and international bodies raise objections but carry no enforcement power against the receiving state's domestic legal order.
% DISAPPEARANCE_RATIONALE: If the sovereignty-based exclusion authority vanished, the receiving state's ability to condition membership, welfare eligibility, and labor-market access on territorial admission would collapse; citizen polities would lose the reserved-access function border control currently provides, labor markets would reorganize, and enforcement infrastructure now employing large numbers of personnel would need to be redeployed or dismantled.
% FOUNDING_PROBLEM: The Westphalian settlement of political authority required a way to determine which persons a given political community was obligated to govern, protect, and provide for, as against all other persons in the world — territorial sovereignty supplied a bright-line answer to an otherwise unbounded distributive and governance problem.
% FOUNDING_PROBLEM_CORROBORATION: International law scholars and UNHCR-aligned bodies attest that the underlying governance-boundary problem remains live but argue the exclusion mechanism has been extended well past what the founding problem requires, particularly against non-threatening economic migrants; sending-state governments and migrant-rights organizations, entities outside the receiving state's beneficiary set, corroborate that current enforcement scope exceeds the original bounded-community rationale and increasingly serves labor-market protectionism and domestic political signaling rather than governance-boundary determination alone.
narrative_ontology:disappearance_verdict(border_legitimacy__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_legitimacy__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__sovereignty_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_legitimacy__sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_legitimacy__sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_legitimacy__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises across the interval (0.42 to 0.68) reflecting the documented expansion of enforcement scope, detention capacity, and criteria stringency over recent decades in major receiving states. Suppression is high throughout and rising (0.58 to 0.79) because the sovereignty reading depends on active, escalating enforcement infrastructure — walls, detention, deportation regimes, biometric tracking — to hold; without that machinery the exclusion claim would not be operative. Theater ratio is modest and slowly rising (0.12 to 0.28): most enforcement activity is functionally real (it does exclude), but a growing share is performative (visible deterrence measures whose primary function is domestic political signaling rather than actual exclusion efficacy). Accessibility collapse is moderately high (0.62): once inside the sovereignty framework's own logic, alternatives for excluded migrants collapse almost entirely — there is no internal appeal to a competing legitimacy claim. Resistance is substantial (0.71): migrants, advocacy networks, and some receiving-state civil society actively contest enforcement, generating litigation, protest, and sanctuary movements.
 *
 * PERSPECTIVAL GAP:
 *   From the receiving state and citizen polity seats, this looks like the ordinary, uncontroversial exercise of self-governance — a rope, or even a mountain-like given of political life. From the excluded migrant and stateless person seats, the same structure computes as high-extraction, high-suppression, actively enforced exclusion with no internal recourse. The engine computing divergent per-seat classifications from the same structural data is the intended output, not an inconsistency to resolve — the sovereignty reading's own coherence for the beneficiary seats coexists with its extractive operation on the payer seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Citizen polity and domestic labor incumbents sit near the beneficiary end: the exclusion function reserves access and labor-market position to them at no direct cost they bear as enforcement targets. The receiving state apparatus is the agenda-setter with analytical/institutional exit — it does not experience the constraint as extraction because it authors and administers it. Excluded migrants, mixed-status families, and stateless persons sit at the full-target end: trapped exit options, powerless structural position, and the sovereignty framework's own terms give them no recognized claim to override exclusion. Border enforcement personnel occupy an intermediate position — administering the constraint (agenda_setter-adjacent) while also bearing real costs (payer-adjacent) from the psychological and legal burden of enforcement, justifying the dual role.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — establishing a bounded basis for political community and governance obligation — remains genuinely live in some form (some allocative boundary is needed for any welfare state or democratic polity to function). But the founding_problem_status is authored as contested because outside corroboration (international law scholarship, UNHCR-aligned analysis, sending-state governments) indicates the mechanism has been extended well past what the founding coordination problem requires, particularly in scope and severity of enforcement against non-threatening economic migrants. This is not classified as pure mandatrophy (a fully dead mandate) because the underlying governance-boundary problem is not dead — it is a tangled rope precisely because a real coordination function persists alongside expanded extraction, not because the coordination function has vanished leaving pure inertial extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_reading_is_one_of_three,
    'This story authors the sovereignty reading of the border_legitimacy kernel. The freedom_of_movement_reading and humanitarian_obligation_reading are structurally distinct constraints with different beneficiary/victim sets and different epsilon values — which reading, if any, should govern actual policy is not resolved by this story.',
    'No empirical resolution exists; this is a genuine normative disagreement about the ground of political obligation and the moral status of territorial boundaries, argued in political philosophy (Walzer vs. Carens being the canonical opposition) and unresolved by international law, which contains elements of all three readings in different treaty regimes.',
    'Under the freedom_of_movement_reading, this same standing arrangement would be authored as a snare with near-total illegitimacy and epsilon near the maximum; under the humanitarian_obligation_reading, the victim set here would split, exempting persecution-fleeing migrants and substantially lowering aggregate epsilon while leaving the economic-migrant-exclusion component largely intact. The sovereignty reading treats the entire exclusion apparatus as legitimately grounded, which is precisely what the sibling readings each deny to different extents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_reading_is_one_of_three, conceptual, 'This story is one of three sibling readings of the border_legitimacy kernel; the disagreement is located at the ground-of-legitimacy layer, not at any empirical fact about enforcement.').

omega_variable(
    sovereignty_scope_creep,
    'Does the sovereignty reading''s founding coordination problem (bounding a political community for self-governance) actually require the current scope and severity of enforcement against non-threatening economic migrants, or has the mechanism expanded well past what the founding problem needs?',
    'Comparative historical analysis of enforcement scope and severity across receiving states relative to periods when the founding problem was equally live but enforcement infrastructure was far less developed (e.g., pre-1980s border regimes in most Western states); if community-boundary coherence held under materially lighter enforcement, current scope is not required by the founding problem.',
    'If enforcement scope substantially exceeds founding necessity, the excess is better classified as extraction riding on a genuine but narrower coordination function, sharpening the tangled_rope classification and supporting policy arguments for reduced enforcement intensity without abandoning the underlying sovereignty claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_scope_creep, empirical, 'Whether current enforcement scope is proportionate to or exceeds the founding governance-boundary problem.').

omega_variable(
    statelessness_limiting_case,
    'Does the sovereignty reading, taken to its logical limit where every state simultaneously exercises exclusion authority, produce an unjustifiable residual category (stateless persons with no legitimate claim anywhere), and does that limiting case count as evidence against the reading''s coherence?',
    'Examine whether the sovereignty reading''s own proponents (e.g., in international law scholarship) treat statelessness as an anomaly requiring special remedy (as the 1954 and 1961 UN statelessness conventions do) or as an acceptable consequence of the framework.',
    'If the sovereignty reading''s own tradition treats statelessness as requiring exceptional remedy, this indicates the reading is not fully self-consistent at its limiting case, which would be evidence for an internal axiom tension rather than full holdability of the sovereignty axiom in the case of stateless persons specifically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(statelessness_limiting_case, conceptual, 'Whether universal simultaneous exclusion (statelessness) is a coherent implication of the sovereignty reading or an internal anomaly.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__sovereignty_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_legitimacy__sovereignty_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(bord_tr_t8, border_legitimacy__sovereignty_reading, theater_ratio, 8, 0.15).
narrative_ontology:measurement(bord_tr_t16, border_legitimacy__sovereignty_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(bord_tr_t24, border_legitimacy__sovereignty_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(bord_tr_t32, border_legitimacy__sovereignty_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement(bord_tr_t40, border_legitimacy__sovereignty_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_legitimacy__sovereignty_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(bord_be_t8, border_legitimacy__sovereignty_reading, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(bord_be_t16, border_legitimacy__sovereignty_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(bord_be_t24, border_legitimacy__sovereignty_reading, base_extractiveness, 24, 0.61).
narrative_ontology:measurement(bord_be_t32, border_legitimacy__sovereignty_reading, base_extractiveness, 32, 0.65).
narrative_ontology:measurement(bord_be_t40, border_legitimacy__sovereignty_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_legitimacy__sovereignty_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(bord_su_t8, border_legitimacy__sovereignty_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(bord_su_t16, border_legitimacy__sovereignty_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(bord_su_t24, border_legitimacy__sovereignty_reading, suppression_requirement, 24, 0.72).
narrative_ontology:measurement(bord_su_t32, border_legitimacy__sovereignty_reading, suppression_requirement, 32, 0.76).
narrative_ontology:measurement(bord_su_t40, border_legitimacy__sovereignty_reading, suppression_requirement, 40, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, freedom_of_movement_reading).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, humanitarian_obligation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings decomposed from the natural-language concept 'border legitimacy,' per the epsilon-invariance principle. sovereignty_reading (this story) authors epsilon ~0.68 on the standing exclusion arrangement, treating exclusion of economic migrants as legitimate and locating the victim set at excluded_economic_migrants/mixed_status_families/stateless_persons. freedom_of_movement_reading would author the same standing arrangement with far higher epsilon and a broader victim set (potentially all excluded migrants including asylum seekers), classifying it as a snare rather than tangled_rope. humanitarian_obligation_reading would author a narrower victim set (excluding persecution-fleeing migrants from the extraction claim) with intermediate epsilon. All three stories describe the SAME standing arrangement (the receiving state's exclusion practice) but assign it different epsilon, different beneficiary/victim structure, and different classification because they instantiate different normative readings of the same contested kernel. They are linked via affects_constraints rather than merged into one story, per the kernel-reading authoring rule.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
