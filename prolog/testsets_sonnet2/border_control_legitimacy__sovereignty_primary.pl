% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__sovereignty_primary, []).

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
 *   constraint_id: border_control_legitimacy__sovereignty_primary
 *   human_readable: Absolute Sovereign Discretion Over Border Exclusion (Sovereignty-Primary Reading)
 *   domain: political_philosophy/international_law/migration
 *
 * SUMMARY:
 *   This constraint instantiates the sovereignty-primary reading of the
 *   contested border-control-legitimacy kernel: the claim that territorial
 *   sovereignty entails an essentially unlimited discretion to exclude
 *   non-citizens, and that this discretion is constitutive of statehood
 *   itself rather than a delegated or rights-limited power. Under this
 *   reading, human rights instruments and international bodies operate as
 *   external moral pressure on a prior authority, not as internal limits
 *   defining what counts as legitimate exercise of that authority. This
 *   produces a large, powerless victim set (excluded asylum seekers,
 *   undocumented resident workers, separated families) whose claims are
 *   structurally pre-empted rather than weighed. The coordination function —
 *   organizing membership, taxation, and governance of a bounded population —
 *   is real, but the sovereignty-primary framing extends that function into
 *   an unbounded discretion that the coordination problem itself does not
 *   require, which is the tangled-rope signature: genuine coordination
 *   bundled with asymmetric extraction sustained by active enforcement.
 *
 * KEY AGENTS:
 *   - receiving_state_apparatus: institutional agenda_setter, administers exclusion under the sovereignty-primary premise
 *   - excluded_asylum_seekers: powerless payer, denied processing on discretionary grounds
 *   - undocumented_resident_workers: powerless payer, trapped in permanent removability
 *   - border_enforcement_industry: powerful beneficiary, revenue scales with enforcement intensity
 *   - international_human_rights_bodies: excluded institutional observer, findings treated as non-binding
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__sovereignty_primary, 0.68).
domain_priors:suppression_score(border_control_legitimacy__sovereignty_primary, 0.81).
domain_priors:theater_ratio(border_control_legitimacy__sovereignty_primary, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__sovereignty_primary, "Absolute Sovereign Discretion Over Border Exclusion (Sovereignty-Primary Reading)").
narrative_ontology:topic_domain(border_control_legitimacy__sovereignty_primary, "political_philosophy/international_law/migration").

domain_priors:requires_active_enforcement(border_control_legitimacy__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__sovereignty_primary, '6d3c29f9-8968-4fa0-b11a-77ea82fa8f2b').
narrative_ontology:cs_kernel_codification('6d3c29f9-8968-4fa0-b11a-77ea82fa8f2b', distributed).
narrative_ontology:cs_authority_grounding('6d3c29f9-8968-4fa0-b11a-77ea82fa8f2b', practice).
narrative_ontology:cs_interpretation_layer_present('6d3c29f9-8968-4fa0-b11a-77ea82fa8f2b').
narrative_ontology:cs_reading_relation('6d3c29f9-8968-4fa0-b11a-77ea82fa8f2b', border_control_legitimacy__freedom_of_movement_primary, forecloses).
narrative_ontology:cs_reading_relation('6d3c29f9-8968-4fa0-b11a-77ea82fa8f2b', border_control_legitimacy__jurisdictional_sovereignty, coexists_with).
narrative_ontology:cs_axiom('6d3c29f9-8968-4fa0-b11a-77ea82fa8f2b', foundational, exclusion_discretion_constitutive_of_statehood).
narrative_ontology:cs_axiom_status(exclusion_discretion_constitutive_of_statehood, holdable).
narrative_ontology:cs_axiom_grounding('6d3c29f9-8968-4fa0-b11a-77ea82fa8f2b', exclusion_discretion_constitutive_of_statehood, conventional).
narrative_ontology:cs_axiom('6d3c29f9-8968-4fa0-b11a-77ea82fa8f2b', secondary, human_rights_norms_external_to_sovereign_authority).
narrative_ontology:cs_axiom_status(human_rights_norms_external_to_sovereign_authority, holdable).
narrative_ontology:cs_axiom_grounding('6d3c29f9-8968-4fa0-b11a-77ea82fa8f2b', human_rights_norms_external_to_sovereign_authority, conventional).
narrative_ontology:cs_reference_frame('6d3c29f9-8968-4fa0-b11a-77ea82fa8f2b', westphalian_absolute_territorial_control).
narrative_ontology:cs_drift_state('6d3c29f9-8968-4fa0-b11a-77ea82fa8f2b', post_1948_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6d3c29f9-8968-4fa0-b11a-77ea82fa8f2b', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__sovereignty_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, receiving_state_apparatus).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, citizen_labor_incumbents).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, border_enforcement_industry).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, excluded_asylum_seekers).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, undocumented_resident_workers).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, family_separated_migrants).
narrative_ontology:constraint_vindicates(border_control_legitimacy__sovereignty_primary, westphalian_state_primacy_doctrine).
narrative_ontology:constraint_vindicates(border_control_legitimacy__sovereignty_primary, citizenship_as_bounded_membership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces exclusion policy under the claim that border control is constitutive of statehood — not a policy choice subject to external override but a precondition of sovereign existence. Administers detention, deportation, and visa regimes, and treats international human rights instruments as advisory constraints on an authority that is prior to and independent of them.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, receiving_state_apparatus, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__sovereignty_primary, receiving_state_apparatus, beneficiary).

% Benefit from reduced labor-market competition and preserved access to public goods and welfare systems that exclusion helps ration. Face no exit cost from the constraint; the constraint's persistence directly serves their material position.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, citizen_labor_incumbents, beneficiary,
    organized, biographical, mobile, national).

% Private contractors, detention operators, and surveillance-technology firms whose revenue scales directly with enforcement intensity. Lobbies to maintain and expand the sovereignty-primary framing because it forecloses political debate about whether enforcement levels are proportionate.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, border_enforcement_industry, beneficiary,
    powerful, generational, arbitrage, national).

% Denied entry or processing on the ground that admission is a discretionary favor rather than an obligation, regardless of the severity of what they are fleeing. Have no standing within the receiving state's legal order to contest the underlying premise that exclusion is a sovereign prerogative rather than a rights-limited power.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, excluded_asylum_seekers, payer,
    powerless, biographical, trapped, global).

% Live and work inside the territory while remaining permanently exposed to removal, wage theft, and non-enforcement of labor protections, because their presence is defined as an ongoing violation of a prerogative that admits no counterbalancing claim. Cannot exit into legal status without political concessions the sovereignty-primary framing structurally withholds.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, undocumented_resident_workers, payer,
    powerless, biographical, trapped, national).

% Parents, children, and spouses split across the border by enforcement action or exclusion decisions that treat family unity as a discretionary consideration subordinate to the exclusion prerogative rather than a competing right of equal standing.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, family_separated_migrants, payer,
    powerless, generational, trapped, national).

% Issue findings and non-refoulement standards that the sovereignty-primary reading treats as external moral commentary rather than binding limits on legitimate state authority. Have no enforcement mechanism against a state that denies their jurisdiction over what it defines as an internal constitutive power.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, international_human_rights_bodies, excluded,
    institutional, civilizational, analytical, global).

% Periodically review whether specific exclusion practices exceed even the sovereignty-primary framing's own bounds (e.g., procedural due process for those already inside the territory), while generally declining to question the underlying premise that exclusion authority itself is absolute and prior to rights claims.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, constitutional_courts, observer,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__sovereignty_primary, constitutional_courts, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_control_legitimacy__sovereignty_primary, diffuse).
narrative_ontology:fixing_cost_class(border_control_legitimacy__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, legible rule for who may enter and remain, allowing a political community to control membership, allocate public goods, and maintain administrative capacity to govern a defined population and territory.
% TRANSFER_FUNCTION: Moves the costs of global displacement, labor mobility restriction, and family separation onto non-citizens and irregular residents, while concentrating the benefits of labor-market protection, fiscal rationing, and political control among citizens and the enforcement apparatus itself.
% ABSENT_VOICES: Excluded migrants and international human rights bodies would object that framing exclusion as constitutive of statehood forecloses any rights-balancing inquiry before it begins; they are structurally absent from the domestic political process that sets exclusion policy, since only citizens vote and only the excluding state adjudicates the claim.
% DISAPPEARANCE_RATIONALE: If the sovereignty-primary premise were abandoned, exclusion decisions would become subject to proportionality review against migrants' claims as a matter of course rather than exception; enforcement budgets, detention capacity, and labor markets would reorganize substantially, and a large population currently held in trapped or excluded status would gain standing to contest specific decisions on their merits.
% FOUNDING_PROBLEM: Nascent territorial states needed a principle to distinguish members from non-members in order to organize taxation, conscription, and self-governance amid contested and overlapping claims to land and population in the post-Westphalian order.
% FOUNDING_PROBLEM_CORROBORATION: The receiving state apparatus and citizen labor incumbents attest the problem remains fully live (unmanaged migration threatens fiscal and political order). International human rights bodies and a substantial body of migration scholarship attest the founding problem of ordering territorial authority has been solved by modern administrative statehood, and that the absolute-discretion reading now functions primarily to insulate enforcement scale and detention economics from rights-based review rather than to solve any live coordination problem.
narrative_ontology:disappearance_verdict(border_control_legitimacy__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__sovereignty_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_control_legitimacy__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__sovereignty_primary, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) and suppression (0.81) are both high and rising because the sovereignty-primary reading forecloses proportionality review as a matter of principle rather than case-by-case outcome — the enforcement apparatus required to sustain an unbounded discretion (detention, deportation infrastructure, biometric tracking) has intensified over the measured interval as migration pressure and political salience have both increased. Theater ratio is moderate (0.32): much enforcement activity is functionally real (actual removals, actual detention), but a growing share is performative deterrence signaling aimed at domestic audiences rather than migration-flow management. Accessibility collapse (0.62) reflects that legal channels for the excluded population have narrowed but not vanished entirely — some asylum and humanitarian pathways persist, which is why this is tangled rope rather than pure snare.
 *
 * PERSPECTIVAL GAP:
 *   From the receiving state apparatus's seat, this is Mountain-adjacent: an irreducible feature of what it means to be a sovereign state, prior to and unconditioned by rights claims. From the excluded and trapped payer seats, the identical structure is an actively enforced extraction that produces concrete, severe, and often irreversible harms (family separation, indefinite removability, denial of refuge). The engine computes these as different seat-level types from the same structural data; the divergence is the finding, not an error to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   The receiving state apparatus and citizen labor incumbents sit near the beneficiary end of directionality — the exclusion prerogative subsidizes their fiscal, labor-market, and political interests at negligible personal cost. The border enforcement industry benefits doubly: financially from enforcement scale and politically from the sovereignty-primary framing's resistance to proportionality challenges that would cap that scale. Excluded asylum seekers, undocumented resident workers, and family-separated migrants sit at the full-target end: trapped exit options, powerless standing, and a framing that denies them even the procedural footing to contest the underlying premise (not merely the specific decision).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — organizing membership and governance capacity for a territorial community — is largely solved by modern administrative statehood; states routinely manage complex population registries, taxation, and services without treating border exclusion as unbounded or unreviewable. The sovereignty-primary reading's insistence on absolute, constitutive discretion outruns what the coordination problem requires, which is the mandatrophy signature: a mandate (order the population, secure governance capacity) has been used to justify an authority (unlimited, rights-immune exclusion) far broader than the mandate needs. Classifying this as tangled_rope rather than snare preserves the genuine coordination residue (some ordering function is real) while flagging the asymmetric extraction riding on top of it — collapsing it to pure snare would erase the coordination function; calling it rope would erase the victim set and enforcement machinery.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutive_vs_delegated_authority,
    'Is exclusion authority genuinely constitutive of statehood (such that a state lacking it is not fully a state), or is it a delegated, historically contingent power that could be redefined without dissolving statehood?',
    'Comparative analysis of political entities (e.g., EU member states under free-movement obligations, historical open-border periods) that retain uncontested statehood while operating under significantly constrained exclusion discretion.',
    'If exclusion authority is not actually constitutive — if states retain full statehood under substantially constrained discretion — the sovereignty-primary reading''s core premise is empirically falsified, and the constraint''s classification shifts toward pure extraction (snare) since the coordination justification collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutive_vs_delegated_authority, conceptual, 'Whether sovereign statehood requires unbounded exclusion discretion or merely some exclusion authority.').

omega_variable(
    framing_selection_evidence,
    'What structural or political signals justify treating the sovereignty-primary reading as the operative one for THIS story rather than jurisdictional_sovereignty (which shares the sovereignty vocabulary but denies the absoluteness claim)?',
    'Examine which reading actually governs current enforcement practice and judicial deference doctrine in the jurisdiction being modeled — courts that decline to review the existence (not just the exercise) of exclusion authority are operating under sovereignty_primary; courts that require proportionality balancing are operating under jurisdictional_sovereignty.',
    'If judicial practice in the modeled jurisdiction actually tracks jurisdictional_sovereignty (balancing required), this story''s claimed_type and beneficiary/victim structure would need to shift toward the sibling reading rather than this one — the two readings would produce different ε and different classifications for the same jurisdiction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_selection_evidence, conceptual, 'Alternative framing (jurisdictional_sovereignty) under-determination and what evidence would distinguish which reading actually operates.').

omega_variable(
    enforcement_proportionality_ambiguity,
    'How much of the measured suppression (0.81) reflects enforcement proportionate to a genuine coordination need (orderly migration management) versus enforcement scaled to sustain the unbounded-discretion premise itself against legal and political challenge?',
    'Compare enforcement intensity and detention capacity across states with similar migration pressure but differing constitutional postures toward exclusion (absolute discretion vs. rights-balanced review); a gap not explained by migration volume indicates premise-defense enforcement.',
    'A large gap would support reclassification toward snare (enforcement primarily defends the extraction, not the coordination); a small gap would support the tangled_rope classification as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_proportionality_ambiguity, empirical, 'Whether enforcement scale tracks coordination need or premise defense.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__sovereignty_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_control_legitimacy__sovereignty_primary, theater_ratio, 0, 0.14).
narrative_ontology:measurement(bord_tr_t8, border_control_legitimacy__sovereignty_primary, theater_ratio, 8, 0.18).
narrative_ontology:measurement(bord_tr_t16, border_control_legitimacy__sovereignty_primary, theater_ratio, 16, 0.21).
narrative_ontology:measurement(bord_tr_t24, border_control_legitimacy__sovereignty_primary, theater_ratio, 24, 0.25).
narrative_ontology:measurement(bord_tr_t32, border_control_legitimacy__sovereignty_primary, theater_ratio, 32, 0.29).
narrative_ontology:measurement(bord_tr_t40, border_control_legitimacy__sovereignty_primary, theater_ratio, 40, 0.32).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_control_legitimacy__sovereignty_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(bord_be_t8, border_control_legitimacy__sovereignty_primary, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(bord_be_t16, border_control_legitimacy__sovereignty_primary, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(bord_be_t24, border_control_legitimacy__sovereignty_primary, base_extractiveness, 24, 0.61).
narrative_ontology:measurement(bord_be_t32, border_control_legitimacy__sovereignty_primary, base_extractiveness, 32, 0.65).
narrative_ontology:measurement(bord_be_t40, border_control_legitimacy__sovereignty_primary, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_control_legitimacy__sovereignty_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(bord_su_t8, border_control_legitimacy__sovereignty_primary, suppression_requirement, 8, 0.63).
narrative_ontology:measurement(bord_su_t16, border_control_legitimacy__sovereignty_primary, suppression_requirement, 16, 0.69).
narrative_ontology:measurement(bord_su_t24, border_control_legitimacy__sovereignty_primary, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(bord_su_t32, border_control_legitimacy__sovereignty_primary, suppression_requirement, 32, 0.78).
narrative_ontology:measurement(bord_su_t40, border_control_legitimacy__sovereignty_primary, suppression_requirement, 40, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__sovereignty_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, freedom_of_movement_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, jurisdictional_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the border_control_legitimacy kernel. sovereignty_primary (this story) authors exclusion discretion as absolute and constitutive, producing a wide victim set among excluded and trapped migrants and classifying as tangled_rope (genuine membership-ordering coordination bundled with enforced asymmetric extraction). freedom_of_movement_primary treats movement as a right sovereignty cannot override, which would classify the same standing enforcement apparatus as substantially more extractive (closer to snare) since it denies any coordination legitimacy to exclusion beyond narrow security exceptions. jurisdictional_sovereignty treats sovereignty as real jurisdictional authority requiring proportionality balancing, which would classify the same apparatus as scaffold-like or rope-like where balancing is genuinely practiced, and tangled_rope or snare where it is not. All three share the same underlying enforcement apparatus as their referent but author different ε, beneficiary/victim structures, and classifications because they differ on what counts as the legitimate scope of the authority being exercised — this is the intended ε-invariance decomposition, not measurement noise.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
