% ============================================================================
% CONSTRAINT STORY: border_legitimacy__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   human_readable: Territorial Sovereignty as Ground for Border Exclusion
 *   domain: political philosophy / migration studies / international law
 *
 * SUMMARY:
 *   This story instantiates the sovereignty reading of the border-legitimacy
 *   kernel: the claim that a state's authority to exclude non-members from
 *   its territory follows directly and legitimately from territorial
 *   sovereignty itself, requiring no further justification beyond the fact of
 *   jurisdiction. Under this reading, the border is a coordination device for
 *   a bounded political community (enabling collective self-rule, resource
 *   allocation, and welfare provision) that necessarily produces exclusion,
 *   and that exclusion is not a defect to be minimized but the mechanism's
 *   intended output. Enforcement against migrants who lack a qualifying claim
 *   is read as the legitimate exercise of a state's basic right, not as a
 *   wrong requiring special justification. The rising ε and
 *   suppression_requirement trajectory reflects the increasing enforcement
 *   infrastructure (biometric screening, offshore detention, third-country
 *   deals, digital surveillance at ports of entry) that sovereignty claims
 *   have been used to justify over the last several decades, even as the
 *   underlying doctrine's content has not changed.
 *
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
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(border_legitimacy__sovereignty_reading, "Territorial Sovereignty as Ground for Border Exclusion").
narrative_ontology:topic_domain(border_legitimacy__sovereignty_reading, "political philosophy / migration studies / international law").

domain_priors:requires_active_enforcement(border_legitimacy__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__sovereignty_reading, '7add778c-a804-4d55-a4bf-96e840c96758').
narrative_ontology:cs_kernel_codification('7add778c-a804-4d55-a4bf-96e840c96758', distributed).
narrative_ontology:cs_authority_grounding('7add778c-a804-4d55-a4bf-96e840c96758', distributed).
narrative_ontology:cs_reading_relation('7add778c-a804-4d55-a4bf-96e840c96758', border_legitimacy__freedom_of_movement_reading, forecloses).
narrative_ontology:cs_reading_relation('7add778c-a804-4d55-a4bf-96e840c96758', border_legitimacy__humanitarian_obligation_reading, influences).
narrative_ontology:cs_axiom('7add778c-a804-4d55-a4bf-96e840c96758', foundational, territorial_jurisdiction_grounds_exclusion_right).
narrative_ontology:cs_axiom_status(territorial_jurisdiction_grounds_exclusion_right, holdable).
narrative_ontology:cs_axiom_grounding('7add778c-a804-4d55-a4bf-96e840c96758', territorial_jurisdiction_grounds_exclusion_right, conventional).
narrative_ontology:cs_axiom('7add778c-a804-4d55-a4bf-96e840c96758', foundational, bounded_membership_necessary_for_self_determination).
narrative_ontology:cs_axiom_status(bounded_membership_necessary_for_self_determination, holdable).
narrative_ontology:cs_axiom_grounding('7add778c-a804-4d55-a4bf-96e840c96758', bounded_membership_necessary_for_self_determination, instrumental).
narrative_ontology:cs_reference_frame('7add778c-a804-4d55-a4bf-96e840c96758', westphalian_territorial_exclusivity).
narrative_ontology:cs_drift_state('7add778c-a804-4d55-a4bf-96e840c96758', contemporary_mass_migration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7add778c-a804-4d55-a4bf-96e840c96758', '').
narrative_ontology:cs_kernel_id(border_legitimacy__sovereignty_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, citizen_polity).
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, receiving_state_apparatus).
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, domestic_labor_incumbents).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, excluded_migrants).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, asylum_seekers_denied_entry).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, transit_country_populations).
narrative_ontology:constraint_vindicates(border_legitimacy__sovereignty_reading, territorial_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(border_legitimacy__sovereignty_reading, state_self_determination_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces admission criteria, staffs border control and detention infrastructure, and adjudicates entry claims. Justifies exclusion authority as an inherent feature of territorial sovereignty and treats the border as a precondition for the polity's capacity to self-govern, tax, and provide domestic goods. Bears the fiscal and administrative cost of enforcement but controls the rules entirely.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, receiving_state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Receives the benefits of controlled membership: access to a bounded welfare state, labor market protections, and a democratic community with a defined demos. Citizens can leave the territory freely (their own exit is unconstrained) while relying on the border to constrain who else may enter and share in those goods.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, citizen_polity, beneficiary,
    organized, generational, mobile, national).

% Benefit from reduced low-wage labor competition where exclusion is enforced against migrants seeking work; their wages and job security are partly a function of restricted labor supply that the border maintains.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, domestic_labor_incumbents, beneficiary,
    moderate, biographical, mobile, national).

% Are denied entry, deported, or detained under the sovereignty claim regardless of their reasons for seeking to cross. They bear the direct cost of exclusion — foreclosed economic opportunity, family separation, exposure to danger in transit or origin countries — and have no legal standing to contest the underlying right to exclude, only its specific application.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Attempt to claim protection under narrow legal exceptions to the sovereignty-based exclusion right; where claims are denied, rejected, or never adjudicated, they are returned to conditions of danger. The sovereignty reading treats their claims as an exception to be managed, not a limit on the underlying authority.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, asylum_seekers_denied_entry, payer,
    powerless, immediate, trapped, global).

% Absorb externalized migration pressure when destination states harden borders — informal settlements, strained local services, and containment arrangements are frequently negotiated or imposed on transit states, who bear costs generated by a border regime they do not control.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, transit_country_populations, payer,
    powerless, biographical, constrained, regional).

% Recognizes territorial sovereignty as a foundational principle of the state system (UN Charter, customary international law) while also maintaining a parallel body of refugee and human rights law that qualifies it. Adjudicates disputes but has no enforcement power over a state's assertion of exclusion authority within its own territory.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, international_legal_order, observer,
    institutional, civilizational, analytical, global).

% Argue that the sovereignty claim is applied selectively and that the moral weight given to territorial jurisdiction over human need is a policy choice, not a self-evident principle. Their objections are heard in courts and advocacy fora but do not participate in setting the underlying admission criteria.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, diaspora_and_migrant_rights_advocates, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A bounded, self-governing political community needs some mechanism to determine membership in order to sustain a demos capable of collective self-rule, allocate finite public goods, and maintain the conditions under which democratic accountability and a functioning welfare state are possible.
% TRANSFER_FUNCTION: Moves the costs of global inequality and displacement away from receiving states and onto excluded individuals and transit states, while moving the benefits of bounded membership (labor market protection, welfare access, political voice) to those already inside the territory.
% ABSENT_VOICES: Excluded migrants and asylum seekers have no vote in the polity that sets the terms of their exclusion; transit states bear externalized costs of destination-state enforcement without a seat in that enforcement's design; diaspora advocates raise these objections from outside the decision-making apparatus.
% DISAPPEARANCE_RATIONALE: If the sovereignty-based exclusion right vanished overnight, states would lose the primary legal and normative basis for controlling entry; labor markets, welfare eligibility, and political membership would have to be renegotiated from scratch, and the entire apparatus of border enforcement, detention, and deportation would lose its justifying premise.
% FOUNDING_PROBLEM: Modern territorial states needed a principle to establish which populations they were responsible for governing, taxing, and defending, and to prevent unlimited claims on a bounded set of resources and political institutions by anyone in the world.
% FOUNDING_PROBLEM_CORROBORATION: Political theorists working outside migration-restrictionist traditions (e.g. in the freedom-of-movement and cosmopolitan literatures) attest that the sovereignty principle, while historically real as a state-formation problem, has been extended far beyond that founding function into a general license for exclusion untethered from any resource-scarcity or governance-capacity constraint; UNHCR and transit-state governments corroborate that current enforcement externalizes costs the sovereignty doctrine does not itself account for.
narrative_ontology:disappearance_verdict(border_legitimacy__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored high (0.68) because the sovereignty reading, taken on its own terms, still produces a large asymmetric transfer: it moves the costs of global displacement onto excluded individuals and transit states while concentrating benefits (labor protection, welfare access, political voice) inside the territorial polity, and it treats humanitarian exceptions as narrow carve-outs rather than limits on the underlying right. Suppression is authored higher still (0.79) because the reading's persistence depends on active enforcement infrastructure — detention, deportation, interdiction — not on migrant consent or acquiescence to the framework's legitimacy. Accessibility collapse (0.62) reflects that once a state asserts the sovereignty ground, few legal avenues remain for contesting entry short of narrow asylum categories. Resistance (0.58) is substantial and organized: migrant rights litigation, sanctuary movements, and international humanitarian law all actively contest applications of this reading even where they do not contest the underlying sovereignty principle.
 *
 * DIRECTIONALITY LOGIC:
 *   Citizens and domestic labor incumbents sit near the beneficiary end: they did not choose their birthright membership but structurally collect the goods the exclusion mechanism protects, and their own exit (emigration) is unconstrained even as others' entry is blocked. The receiving state apparatus is the agenda-setter with no meaningful directionality of its own — it administers rather than bears the transfer. Excluded migrants and denied asylum seekers sit at the full-target end: trapped exit options, no standing to contest the underlying doctrine, and the direct bearer of the cost the border imposes. Transit country populations are victims by externalization rather than by direct targeting — the sovereignty claim of a destination state routes costs through a third territory that has no seat in that state's admission policy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — establishing a bounded political community capable of self-governance and resource allocation — has not disappeared, which is why founding_problem_status is authored as contested rather than dead: the sovereignty reading's proponents can correctly point to a live coordination need. But the scale and intensity of enforcement (rising suppression_requirement) has expanded well past what the founding problem requires, suggesting a Tangled Rope reading is more accurate than either a pure Rope (the coordination story alone) or a pure Snare (denying any coordination function exists). Classifying this as Tangled Rope rather than Snare preserves the real coordination function (self-governing political communities are not fictions) while still registering the asymmetric extraction that the sovereignty doctrine, as currently enforced, produces.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_scope_ambiguity,
    'Does territorial sovereignty, as a principle, actually entail a right to exclude non-threatening individuals seeking peaceful entry, or does it only entail a right to govern within the territory once entry has occurred — with exclusion authority being a separate, contestable extension?',
    'Comparative analysis of sovereignty''s historical and juridical content prior to its extension to migration control (e.g. Westphalian sovereignty as non-interference in internal governance vs. modern border control as external population management) would clarify whether exclusion is intrinsic to or merely appended to the sovereignty concept.',
    'If exclusion is not intrinsic to sovereignty but a later addition, the sovereignty_reading''s foundational axiom is weaker than claimed and the constraint moves toward snare; if genuinely intrinsic, the tangled_rope classification (real coordination function plus real extraction) is well-grounded.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_scope_ambiguity, conceptual, 'Whether exclusion authority is intrinsic to or merely historically appended to territorial sovereignty.').

omega_variable(
    reading_relationship_to_siblings,
    'Is this reading a genuine alternative moral framework to the freedom_of_movement_reading and humanitarian_obligation_reading, or is it better understood as the status quo that those readings are built to critique — meaning the three readings are not symmetric competitors but one default plus two challenger positions?',
    'Track which reading current international law and state practice actually implement as baseline versus which readings appear primarily in normative/critical literature arguing against that baseline.',
    'If sovereignty_reading is the operative default, its ε should be read as measuring the actually-operating global border regime, while the sibling readings measure counterfactual or partially-implemented alternatives (the humanitarian_obligation_reading is partially codified in refugee law; freedom_of_movement_reading is almost nowhere codified). This affects how directly comparable the three readings'' classifications are.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_relationship_to_siblings, conceptual, 'Whether the three kernel readings are symmetric alternatives or a default-plus-challengers structure.').

omega_variable(
    coalition_power_of_excluded_migrants,
    'Given that excluded migrants are individually powerless and trapped, can transnational advocacy coalitions, sending-state diplomatic pressure, or transit-state non-cooperation aggregate into meaningful counter-power against sovereignty-based exclusion?',
    'Case studies of successful transit-state or sending-state leverage (e.g. remittance-dependent states negotiating labor migration quotas) versus cases where such leverage failed to alter destination-state exclusion policy.',
    'If aggregation is effective in some corridors, the powerless/trapped classification should be qualified for those specific migration corridors even though it holds generally.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coalition_power_of_excluded_migrants, empirical, 'Whether coalition or diplomatic leverage can offset individual migrant powerlessness.').


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
narrative_ontology:measurement(bord_be_t8, border_legitimacy__sovereignty_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(bord_be_t16, border_legitimacy__sovereignty_reading, base_extractiveness, 16, 0.57).
narrative_ontology:measurement(bord_be_t24, border_legitimacy__sovereignty_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(bord_be_t32, border_legitimacy__sovereignty_reading, base_extractiveness, 32, 0.66).
narrative_ontology:measurement(bord_be_t40, border_legitimacy__sovereignty_reading, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_legitimacy__sovereignty_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(bord_su_t8, border_legitimacy__sovereignty_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(bord_su_t16, border_legitimacy__sovereignty_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(bord_su_t24, border_legitimacy__sovereignty_reading, suppression_requirement, 24, 0.73).
narrative_ontology:measurement(bord_su_t32, border_legitimacy__sovereignty_reading, suppression_requirement, 32, 0.77).
narrative_ontology:measurement(bord_su_t40, border_legitimacy__sovereignty_reading, suppression_requirement, 40, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, freedom_of_movement_reading).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, humanitarian_obligation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling readings of the border_legitimacy kernel, each authored as a separate constraint per the epsilon-invariance principle: sovereignty_reading (this file, ε=0.68, tangled_rope), freedom_of_movement_reading (ε expected much higher on the sovereignty-defended exclusion mechanism, likely snare from that reading's own lights), and humanitarian_obligation_reading (ε expected intermediate, likely tangled_rope with a narrower victim set limited to wrongly-denied asylum claims). The three do not share an ε value because each reading assesses a different normative baseline for what counts as legitimate exclusion, even though all three describe the same underlying physical border-enforcement apparatus.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
