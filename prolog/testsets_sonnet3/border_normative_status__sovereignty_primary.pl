% ============================================================================
% CONSTRAINT STORY: border_normative_status__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__sovereignty_primary, []).

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
 *   constraint_id: border_normative_status__sovereignty_primary
 *   human_readable: Sovereignty-Primary Reading of Border Legitimacy
 *   domain: political philosophy / international law / migration
 *
 * SUMMARY:
 *   This story instantiates the sovereignty-primary reading of the contested
 *   border_normative_status kernel: territorial boundaries are legitimate
 *   instruments of collective self-determination, and states possess a
 *   foundational (not merely derivative or conditional) authority to exclude
 *   non-members. Under this reading, excluded migrants, denied asylum
 *   seekers, and undocumented residents constitute the victim set; border
 *   enforcement is a legitimate exercise of collective self-governance rather
 *   than a constraint requiring extraordinary justification; and the
 *   displacement or harm to excluded persons is treated within the reading's
 *   own logic as an externality to the self-determination claim, not as a
 *   competing foundational right that must be weighed against it. The sibling
 *   readings — freedom_primary (movement as a fundamental right borders
 *   impermissibly restrict) and qualified_sovereignty (sovereignty retained
 *   but bounded by proportionality and human rights) — are NOT represented
 *   here; they are separate constraints with their own ε, beneficiary/victim
 *   sets, and classifications, linked only via network and
 *   cs_structure.reading_relations.
 *
 * KEY AGENTS:
 *   - incumbent_state_apparatus: agenda_setter, administers and justifies the exclusion authority
 *   - citizen_polity: beneficiary, receives self-governance and membership goods
 *   - excluded_migrants: primary payer, bears exclusion with no standing
 *   - asylum_seekers_denied_entry: payer, protection claims subordinated to sovereign discretion
 *   - undocumented_residents_inside_territory: payer, lives under permanent removal exposure
 *   - human_rights_monitoring_bodies: observer, asserts a bounding claim this reading subordinates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__sovereignty_primary, 0.58).
domain_priors:suppression_score(border_normative_status__sovereignty_primary, 0.72).
domain_priors:theater_ratio(border_normative_status__sovereignty_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, extractiveness, 0.58).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(border_normative_status__sovereignty_primary, "Sovereignty-Primary Reading of Border Legitimacy").
narrative_ontology:topic_domain(border_normative_status__sovereignty_primary, "political philosophy / international law / migration").

domain_priors:requires_active_enforcement(border_normative_status__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__sovereignty_primary, 'fbaf45f7-6d29-41ce-a591-db85f330a3e4').
narrative_ontology:cs_kernel_codification('fbaf45f7-6d29-41ce-a591-db85f330a3e4', distributed).
narrative_ontology:cs_authority_grounding('fbaf45f7-6d29-41ce-a591-db85f330a3e4', practice).
narrative_ontology:cs_interpretation_layer_present('fbaf45f7-6d29-41ce-a591-db85f330a3e4').
narrative_ontology:cs_reading_relation('fbaf45f7-6d29-41ce-a591-db85f330a3e4', border_normative_status__freedom_primary, forecloses).
narrative_ontology:cs_reading_relation('fbaf45f7-6d29-41ce-a591-db85f330a3e4', border_normative_status__qualified_sovereignty, influences).
narrative_ontology:cs_axiom('fbaf45f7-6d29-41ce-a591-db85f330a3e4', foundational, collective_self_determination_grounds_exclusion_right).
narrative_ontology:cs_axiom_status(collective_self_determination_grounds_exclusion_right, holdable).
narrative_ontology:cs_axiom_grounding('fbaf45f7-6d29-41ce-a591-db85f330a3e4', collective_self_determination_grounds_exclusion_right, deontological).
narrative_ontology:cs_axiom('fbaf45f7-6d29-41ce-a591-db85f330a3e4', foundational, membership_boundary_prior_to_individual_entry_claim).
narrative_ontology:cs_axiom_status(membership_boundary_prior_to_individual_entry_claim, holdable).
narrative_ontology:cs_axiom_grounding('fbaf45f7-6d29-41ce-a591-db85f330a3e4', membership_boundary_prior_to_individual_entry_claim, conventional).
narrative_ontology:cs_reference_frame('fbaf45f7-6d29-41ce-a591-db85f330a3e4', westphalian_territorial_sovereignty).
narrative_ontology:cs_drift_state('fbaf45f7-6d29-41ce-a591-db85f330a3e4', post_1951_refugee_convention_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('fbaf45f7-6d29-41ce-a591-db85f330a3e4', '').
narrative_ontology:cs_kernel_id(border_normative_status__sovereignty_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, citizen_polity).
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, incumbent_state_apparatus).
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, domestic_labor_market_incumbents).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, asylum_seekers_denied_entry).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, undocumented_residents_inside_territory).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, border_enforcement_workforce).
narrative_ontology:constraint_vindicates(border_normative_status__sovereignty_primary, collective_self_determination_doctrine).
narrative_ontology:constraint_vindicates(border_normative_status__sovereignty_primary, state_territorial_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines, patrols, and adjudicates who may cross the territorial line, and justifies this as the exercise of a foundational collective right to self-determination. Administers visa regimes, detention, and removal. Its authority is precisely the thing this reading declares legitimate rather than merely factual.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, incumbent_state_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Receives the good of a bounded, self-governing community that can set its own membership terms, labor protections, and welfare distribution without external claimants. Retains full internal mobility and exit; the constraint operates on others, not on them.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, citizen_polity, beneficiary,
    organized, generational, mobile, national).

% Benefits from reduced labor-market competition and preserved bargaining position because entry is gated. Can exit the labor market or relocate within the territory; never faces the exclusion mechanism itself.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, domestic_labor_market_incumbents, beneficiary,
    moderate, biographical, mobile, national).

% Denied entry or presence on the territory on the ground that the polity has a prior, foundational right to exclude. Bears the full cost of the boundary — foreclosed livelihood, family separation, or return to danger — with no standing inside the deciding forum. Under this reading, their claim to enter does not rise to the level that would require the state's authority to yield.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Seek protection but are turned back, detained, or processed under regimes designed primarily to preserve the state's discretion to exclude. Their protection claims are adjudicated inside a framework this reading treats as subordinate to the prior sovereign prerogative, not as a competing foundational right.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, asylum_seekers_denied_entry, payer,
    powerless, immediate, trapped, global).

% Live inside the boundary without recognized status, subject to removal at any time as an expression of the same foundational exclusion authority. Cannot access the protections the citizen_polity enjoys; exit means self-deportation into the same conditions that drove departure.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, undocumented_residents_inside_territory, payer,
    powerless, biographical, trapped, national).

% Staffs the physical and administrative apparatus that operationalizes the exclusion authority — patrol, detention, adjudication. Draws salary and institutional standing from the enforcement function; could exit to other work, unlike those it processes.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, border_enforcement_workforce, agenda_setter,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(border_normative_status__sovereignty_primary, border_enforcement_workforce, beneficiary).

% Bear the downstream effects of blocked emigration (remittance loss, population pressure) but have no standing in the receiving state's determination of its own membership rules — this reading treats their interest as external to the question of legitimate exclusion.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, sending_state_governments, excluded,
    moderate, generational, constrained, national).

% Document conditions at borders and in detention, and periodically assert that exclusion authority is bounded by human rights obligations — a claim this reading subordinates to the prior sovereignty premise rather than treats as a co-equal constraint.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, human_rights_monitoring_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__sovereignty_primary, citizen_polity).
narrative_ontology:fixing_cost_class(border_normative_status__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables a bounded political community to set membership terms, allocate scarce public goods (welfare, representation, land) among a defined population, and sustain the conditions for collective self-governance without unlimited external claimants.
% TRANSFER_FUNCTION: Moves the cost of population control from the receiving polity onto individuals seeking entry: labor-market protection and fiscal exclusivity flow to citizens and incumbents; displacement, detention, family separation, and foreclosed mobility flow to excluded migrants, asylum seekers, and undocumented residents.
% ABSENT_VOICES: Excluded migrants and asylum seekers are the parties most affected by the boundary and have no vote, no standing, and generally no forum in which the receiving state's foundational claim to exclude is itself contestable from their side; sending states are treated as having no legitimate interest in the receiving state's membership rules.
% DISAPPEARANCE_RATIONALE: If the sovereignty-primary reading were displaced overnight (e.g., by a global regime requiring entry justification rather than exclusion justification), enforcement infrastructure, labor-market protections premised on closure, and welfare-eligibility rules keyed to citizenship would all require reconstruction; excluded populations would gain standing they currently lack.
% FOUNDING_PROBLEM: Modern states arose partly to solve the problem of who is answerable to whom for collective decisions — taxation, defense, law — which requires a defined membership so obligations and benefits attach to identifiable persons rather than an unbounded population.
% FOUNDING_PROBLEM_CORROBORATION: Political theorists in the self-determination tradition (attesting from within the reading) hold the problem live and load-bearing. Independent migration scholars and UNHCR-adjacent researchers, who are outside the beneficiary set, argue the same founding problem could be solved by weaker, proportionality-bounded exclusion regimes (the qualified_sovereignty reading) — i.e., that the founding problem does not require this reading's strong, foundational version of the exclusion right, only some version of bounded membership.
narrative_ontology:disappearance_verdict(border_normative_status__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__sovereignty_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_normative_status__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__sovereignty_primary, 0.58, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58) reflects that real costs (foreclosed livelihood, family separation, detention) are imposed on a powerless population to secure a good (bounded self-governance) for an organized one — substantial but not maximal, because the coordination function (a genuine collective-action problem: who is a member, who bears obligations) is real and not merely pretextual. Suppression (0.72) is high because the exclusion authority depends on active enforcement — patrols, detention, removal — not on voluntary compliance; this is a raw structural property and is NOT scaled by scope or power in the authored value, only in the engine's downstream computation of effective extraction. Accessibility collapse (0.68) is moderately high: once a state asserts the sovereignty-primary premise, alternatives (unrestricted entry, individualized proportionality review) are largely foreclosed as a matter of the reading's own logic, though not as completely as a genuine natural law. Resistance (0.6) captures ongoing legal challenge, asylum litigation, and human-rights advocacy against the exclusion regime — real resistance, consistent with a contested constructed arrangement rather than an uncontested mountain.
 *
 * DIRECTIONALITY LOGIC:
 *   citizen_polity and domestic_labor_market_incumbents are declared beneficiaries with mobile exit — the constraint operates on others, subsidizing their position. incumbent_state_apparatus and border_enforcement_workforce administer the exclusion and derive institutional standing and salary from it, placing them near the agenda-setting/beneficiary end. excluded_migrants, asylum_seekers_denied_entry, and undocumented_residents_inside_territory are declared victims with trapped exit — under this reading, their claims do not generate a standing that would require the state's authority to yield, so they sit at the high-d, high-effective-extraction end of the derivation. sending_state_governments are excluded rather than coordinated: their interest is treated by this reading as external to the question of legitimate exclusion, which is itself the structural point the reading makes.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — a bounded population accountable for collective obligations — is genuinely old and arguably still live in some form (states do need some membership boundary to allocate obligations). The tangled_rope classification (rather than snare) is deliberate: it holds open that a real coordination function persists even as the current exclusion apparatus imposes severe, asymmetric costs on a powerless population that has no voice in setting the terms. Classifying this as a pure snare would erase the coordination claim entirely; classifying it as a rope would erase the victim set the reading itself concedes exists. Whether the sovereignty-primary version of this authority (as opposed to a proportionality-bounded version) is still required by the founding problem is exactly the site of the kernel contest — see the sibling readings.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_primary_vs_sibling_readings_location,
    'Is the sovereignty-primary premise (foundational, non-conditional exclusion authority) itself defensible, or does the underlying founding problem (bounded accountability for collective obligations) only require the weaker qualified_sovereignty version — proportional, rights-bounded exclusion?',
    'Comparative institutional analysis: do states operating under proportionality-bounded exclusion regimes (approximating qualified_sovereignty) fail to solve the membership/accountability problem relative to states asserting stronger foundational exclusion authority? Track outcomes on fiscal sustainability, democratic legitimacy, and rights compliance.',
    'If proportionality-bounded regimes solve the founding problem equally well, this reading''s stronger sovereignty claim is doing extractive work beyond what coordination requires, strengthening the tangled_rope reading toward snare. If the stronger claim is functionally necessary, the coordination component is more load-bearing than the metrics currently credit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_primary_vs_sibling_readings_location, conceptual, 'Location of the sovereignty_primary vs qualified_sovereignty disagreement: whether the founding problem requires foundational (not merely conditional) exclusion authority.').

omega_variable(
    displacement_externality_status,
    'This reading treats harm to excluded migrants as an externality to the self-determination claim rather than a competing foundational right. Is that framing itself defensible, or does it presuppose the very point in contest with freedom_primary?',
    'Examine whether any version of the sovereignty-primary reading can be stated without assuming, rather than arguing for, the subordination of migrant interests — i.e., whether the externality treatment is argued or merely stipulated.',
    'If the externality framing is merely stipulated, the reading''s ε may understate the real contestedness of the harm it authorizes; if independently argued (e.g., via associative or democratic-legitimacy theory), the framing is more structurally stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_externality_status, conceptual, 'Whether treating migrant displacement as externality is argued or assumed within this reading.').

omega_variable(
    enforcement_necessity_vs_ratchet,
    'Does the rising suppression_requirement trajectory reflect a genuinely escalating need to defend a stable sovereignty claim, or an enforcement ratchet where infrastructure investment outpaces any change in the underlying self-determination interest it claims to protect?',
    'Compare growth in enforcement budgets/detention capacity/removal rates against migration flow volumes and threat indicators the state itself cites as justification.',
    'If enforcement growth outpaces flow/threat growth, the rising suppression figure reflects institutional entrenchment (favoring reclassification pressure toward snare) rather than proportionate response to a stable coordination need.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_necessity_vs_ratchet, empirical, 'Whether escalating enforcement tracks a genuine need or an enforcement ratchet.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__sovereignty_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_normative_status__sovereignty_primary, theater_ratio, 0, 0.18).
narrative_ontology:measurement(bord_tr_t8, border_normative_status__sovereignty_primary, theater_ratio, 8, 0.2).
narrative_ontology:measurement(bord_tr_t16, border_normative_status__sovereignty_primary, theater_ratio, 16, 0.22).
narrative_ontology:measurement(bord_tr_t24, border_normative_status__sovereignty_primary, theater_ratio, 24, 0.25).
narrative_ontology:measurement(bord_tr_t32, border_normative_status__sovereignty_primary, theater_ratio, 32, 0.27).
narrative_ontology:measurement(bord_tr_t40, border_normative_status__sovereignty_primary, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_normative_status__sovereignty_primary, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(bord_be_t8, border_normative_status__sovereignty_primary, base_extractiveness, 8, 0.47).
narrative_ontology:measurement(bord_be_t16, border_normative_status__sovereignty_primary, base_extractiveness, 16, 0.51).
narrative_ontology:measurement(bord_be_t24, border_normative_status__sovereignty_primary, base_extractiveness, 24, 0.55).
narrative_ontology:measurement(bord_be_t32, border_normative_status__sovereignty_primary, base_extractiveness, 32, 0.57).
narrative_ontology:measurement(bord_be_t40, border_normative_status__sovereignty_primary, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_normative_status__sovereignty_primary, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(bord_su_t8, border_normative_status__sovereignty_primary, suppression_requirement, 8, 0.56).
narrative_ontology:measurement(bord_su_t16, border_normative_status__sovereignty_primary, suppression_requirement, 16, 0.62).
narrative_ontology:measurement(bord_su_t24, border_normative_status__sovereignty_primary, suppression_requirement, 24, 0.67).
narrative_ontology:measurement(bord_su_t32, border_normative_status__sovereignty_primary, suppression_requirement, 32, 0.7).
narrative_ontology:measurement(bord_su_t40, border_normative_status__sovereignty_primary, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__sovereignty_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, border_normative_status__freedom_primary).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, border_normative_status__qualified_sovereignty).

% DUAL FORMULATION NOTE:
% This story is one of three constraints decomposed from the natural-language 'border legitimacy' claim, per the ε-invariance principle: sovereignty_primary (this file, tangled_rope, ε=0.58), freedom_primary (inverted beneficiary/victim structure treating exclusion as the extractive act requiring justification, expected much higher ε for the exclusion arrangement from that reading's lights), and qualified_sovereignty (proportionality-bounded exclusion, expected materially lower ε than this reading due to narrower victim set and rights-review constraint on enforcement). All three share the same kernel (border_normative_status) but instantiate structurally distinct constraints with distinct ε values, beneficiary/victim sets, and classifications — they are not the same constraint viewed from different angles.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
