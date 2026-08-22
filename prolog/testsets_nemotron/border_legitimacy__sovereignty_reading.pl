% ============================================================================
% CONSTRAINT STORY: border_legitimacy__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-25
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: border_legitimacy__sovereignty_reading
 *   human_readable: Sovereignty-Based Border Exclusion Authority
 *   domain: political/migration/legal
 *
 * SUMMARY:
 *   This constraint story captures the sovereignty reading of border
 *   legitimacy: the state's authority to exclude derives from its territorial
 *   sovereignty, and this exclusion is a legitimate exercise of
 *   self-determination. The constraint operates as a snare — high extraction
 *   from excluded migrants, sustained by active enforcement (detention,
 *   deportation, interdiction, deterrence), with the coordination story
 *   (collective self-determination) serving as cover for extraction that
 *   primarily benefits the state-citizenry coalition. The kernel contest:
 *   three readings of border legitimacy share the label 'border authority'
 *   but instantiate structurally distinct constraints with different
 *   beneficiary/victim sets and different ε values. This story authors ONLY
 *   the sovereignty reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_legitimacy__sovereignty_reading, 0.78).
domain_priors:suppression_score(border_legitimacy__sovereignty_reading, 0.85).
domain_priors:theater_ratio(border_legitimacy__sovereignty_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(border_legitimacy__sovereignty_reading, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_legitimacy__sovereignty_reading, snare).
narrative_ontology:human_readable(border_legitimacy__sovereignty_reading, "Sovereignty-Based Border Exclusion Authority").
narrative_ontology:topic_domain(border_legitimacy__sovereignty_reading, "political/migration/legal").

domain_priors:requires_active_enforcement(border_legitimacy__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_legitimacy__sovereignty_reading, '22796a47-0ebf-451e-9f49-a489871b1b7a').
narrative_ontology:cs_kernel_codification('22796a47-0ebf-451e-9f49-a489871b1b7a', formalized).
narrative_ontology:cs_authority_grounding('22796a47-0ebf-451e-9f49-a489871b1b7a', lineage).
narrative_ontology:cs_interpretation_layer_present('22796a47-0ebf-451e-9f49-a489871b1b7a').
narrative_ontology:cs_reading_relation('22796a47-0ebf-451e-9f49-a489871b1b7a', border_legitimacy__freedom_of_movement_reading, forecloses).
narrative_ontology:cs_reading_relation('22796a47-0ebf-451e-9f49-a489871b1b7a', border_legitimacy__humanitarian_obligation_reading, influences).
narrative_ontology:cs_axiom('22796a47-0ebf-451e-9f49-a489871b1b7a', foundational, state_absolute_exclusion_authority).
narrative_ontology:cs_axiom_status(state_absolute_exclusion_authority, holdable).
narrative_ontology:cs_axiom_grounding('22796a47-0ebf-451e-9f49-a489871b1b7a', state_absolute_exclusion_authority, conventional).
narrative_ontology:cs_axiom('22796a47-0ebf-451e-9f49-a489871b1b7a', foundational, membership_requires_sovereign_consent).
narrative_ontology:cs_axiom_status(membership_requires_sovereign_consent, holdable).
narrative_ontology:cs_axiom_grounding('22796a47-0ebf-451e-9f49-a489871b1b7a', membership_requires_sovereign_consent, conventional).
narrative_ontology:cs_reference_frame('22796a47-0ebf-451e-9f49-a489871b1b7a', westphalian_sovereign_order).
narrative_ontology:cs_drift_state('22796a47-0ebf-451e-9f49-a489871b1b7a', contemporary_human_rights_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('22796a47-0ebf-451e-9f49-a489871b1b7a', '').
narrative_ontology:cs_kernel_id(border_legitimacy__sovereignty_reading, border_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, territorial_state).
narrative_ontology:constraint_beneficiary(border_legitimacy__sovereignty_reading, national_citizenry).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, excluded_migrants).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, asylum_seekers_denied_entry).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, stateless_persons).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, national_citizenry).
narrative_ontology:constraint_victim(border_legitimacy__sovereignty_reading, border_enforcement_agencies).
narrative_ontology:constraint_vindicates(border_legitimacy__sovereignty_reading, territorial_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(border_legitimacy__sovereignty_reading, state_self_determination_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claims and enforces exclusive authority over border crossing decisions. Derives legitimacy from territorial sovereignty doctrine and international recognition. Collects control over membership, labor markets, and security architecture. Can modify border policy unilaterally within international law constraints. Exit from this role means ceding sovereign authority — structurally improbable for a functioning state.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, territorial_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Receives the primary benefits of exclusion: labor market protection, welfare state sustainability, cultural continuity, and democratic self-governance within bounded polity. Also bears costs: enforcement taxation, economic opportunity loss from restricted migration, moral injury from exclusionary policies. Exit requires emigration — constrained by other states' border regimes.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, national_citizenry, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__sovereignty_reading, national_citizenry, payer).

% Bear the full costs of exclusion: denied access to labor markets, safety, family reunification, and political membership. Face physical danger in transit, detention, and return. Have no voice in the policies that exclude them. Exit options are structurally blocked — the constraint itself is the barrier to exit. Some achieve entry through irregular channels but remain precarious.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Flee persecution, conflict, or disaster and present at borders seeking protection. Are denied entry or returned under sovereignty-based exclusion regimes that prioritize state discretion over non-refoulement obligations. Bear existential costs: return to persecution, indefinite detention, family separation. Exit from the constraint is the protection they seek — the constraint is the barrier to that exit.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, asylum_seekers_denied_entry, payer,
    powerless, immediate, trapped, global).

% Lack any recognized nationality and are excluded from all state membership systems. Border regimes presuppose citizenship; statelessness is the gap where sovereignty's logic produces permanent exclusion. Bear generational costs: no legal residence, no work authorization, no document access, no diplomatic protection. Identity-locked because their legal non-existence is produced by the same sovereign system that excludes them.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, stateless_persons, payer,
    powerless, generational, identity_locked, global).

% Monitor, document, and adjudicate border practices against human rights treaties. Issue findings, recommendations, and occasional binding rulings. Lack enforcement power against sovereign states. Their authority is analytical and normative — they observe the constraint's operation from outside the sovereign enclosure.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, human_rights_institutions, observer,
    institutional, generational, analytical, global).

% Implement exclusion policy through detention, deportation, interdiction, and deterrence. Capture institutional resources, mission authority, and bureaucratic expansion from enforcement. Also bear costs: moral injury, operational hazards, legal liability, public scrutiny. Constrained exit — transfer within state apparatus or leave public service.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, border_enforcement_agencies, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__sovereignty_reading, border_enforcement_agencies, payer).

% Provides the normative framework (UN Charter, Refugee Convention, human rights treaties) that both recognizes sovereign border authority and limits it. The tension between sovereignty and human rights is the kernel's contest. The legal order itself is excluded from enforcement — it can declare but not compel compliance.
narrative_ontology:constraint_stakeholder(border_legitimacy__sovereignty_reading, international_legal_order, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(border_legitimacy__sovereignty_reading, international_legal_order, excluded).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective self-determination by defining the bounded political community within which democratic authority, resource distribution, and legal accountability operate. Solves the problem of who constitutes the 'self' in self-government.
% TRANSFER_FUNCTION: Moves the right of entry, residence, labor market access, and political membership from excluded non-citizens to the state and its citizenry. The transfer is enforced through physical interdiction, legal denial, and administrative exclusion.
% ABSENT_VOICES: Excluded migrants, asylum seekers, and stateless persons are structurally absent from the sovereign decision-making that produces their exclusion. Their voices would challenge the legitimacy of a system that binds them without consent. They are absent because the constraint itself — border exclusion — is the mechanism of their absence.
% DISAPPEARANCE_RATIONALE: If sovereign exclusion authority vanished overnight, global human mobility would reorganize around individual choice rather than state permission. Labor markets, welfare systems, democratic constituencies, and security architectures would all require fundamental restructuring. The world of bounded political communities would dissolve into something unrecognizable.
% FOUNDING_PROBLEM: The Westphalian state system required a principle to allocate authority over territory and population after religious wars. Sovereign border control was the answer: each state decides who enters, establishing the membership basis for internal legitimacy and external non-interference.
% FOUNDING_PROBLEM_CORROBORATION: International relations scholars (Krasner, Jackson) attest the Westphalian founding problem — ordering authority after imperial/religious collapse — is historically real but argue its migration-specific application is contested. Migration historians (Torpey, Fitzgerald) document that general border exclusion is a 20th-century construction, not a Westphalian original. Human rights advocates (UNHCR, Amnesty) attest the founding problem of state ordering does not justify the current scale of exclusionary violence. No corroboration exists from outside the benefiting state-citizenry coalition that the founding problem requires today's exclusion regime.
narrative_ontology:disappearance_verdict(border_legitimacy__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(border_legitimacy__sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_legitimacy__sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(border_legitimacy__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(border_legitimacy__sovereignty_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Base extractiveness (0.78) is high because the constraint transfers life-changing opportunities (safety, labor, family, membership) from the globally powerless to the nationally organized, and the transfer is enforced not by consent but by physical and legal coercion. Suppression (0.85) is very high because the constraint's persistence depends on actively preventing exit from the excluded condition — the border regime IS the barrier to the alternatives (entry, asylum, citizenship). Theater ratio (0.22) is moderate-low: the self-determination coordination function is real but diminishingly proportionate to the enforcement apparatus. Accessibility collapse (0.68) reflects that once the sovereign exclusion framework is accepted, alternatives (open borders, freedom of movement, humanitarian admission) appear politically unimaginable. Resistance (0.42) is moderate: migrants resist individually and collectively, but structural power asymmetry limits effective resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the state/citizenry seat, the constraint appears as legitimate coordination (rope-like): we govern ourselves by deciding who joins us. From the excluded migrant seat, it appears as pure extraction enforced by violence (snare): you take our life-chances to protect your privilege. From the enforcement agency seat, it appears as mission with moral injury (tangled_rope): we do the work but the work damages us. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The territorial state (agenda_setter, institutional, arbitrage exit) sits at d ≈ 0.05 — full beneficiary, the constraint subsidizes its authority. National citizenry (beneficiary/payer, organized, constrained exit) at d ≈ 0.3 — net beneficiaries but bear enforcement costs and moral costs. Excluded migrants, asylum seekers, stateless persons (payers, powerless, trapped/identity_locked) at d ≈ 0.95-1.0 — full targets, the constraint extracts their life-chances. Border enforcement agencies (agenda_setter/payer, organized, constrained) at d ≈ 0.4 — they administer and benefit institutionally but bear moral/operational costs. Human rights institutions and international legal order (observers, analytical exit) at d ≈ 0.5 — analytical seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Westphalian ordering) is historically real but the current constraint has metastasized far beyond its founding justification. The coordination function (bounded self-government) no longer requires the scale and brutality of contemporary exclusion regimes. The mandate has atrophied into extraction — but the extraction is so structurally central to the state-citizenry coalition that it cannot be acknowledged as mandatrophy. The sovereignty reading denies mandatrophy by declaring the founding problem perpetually live: self-determination always requires exclusion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_human_rights_primacy,
    'Does territorial sovereignty structurally require the right to exclude, or is the exclusion right a contingent political choice that sovereignty could exist without?',
    'Counterfactual analysis of sovereign states that have adopted open borders or freedom of movement regimes (e.g., EU internal borders, historical open-border periods) — do they lose sovereign authority in other domains?',
    'If sovereignty does not require exclusion, the constraint''s claimed coordination function is contingent, not necessary — supporting reclassification from snare toward tangled_rope (coordination function real but extraction excessive). If sovereignty requires exclusion, the coordination function is structural and the snare classification reflects a genuine tragic tradeoff.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_human_rights_primacy, conceptual, 'Whether the exclusion right is constitutive of sovereignty or a contingent policy choice.').

omega_variable(
    exclusion_as_extraction_measurement,
    'How should extractiveness be measured when the ''resource'' extracted is not a fungible good but life-chances (safety, membership, family unity, political voice)?',
    'Develop non-monetary extraction metrics: years of life lost to exclusion, capability deprivation indices, intergenerational mobility gaps. Compare against the citizenry''s actual welfare gains from exclusion.',
    'If extraction is systematically undermeasured by current metrics (which track monetary/resource flows), the true ε is higher than authored — reinforcing snare classification. If citizenry gains are also undermeasured, the extraction/benefit ratio may shift.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exclusion_as_extraction_measurement, empirical, 'Measurement adequacy of extractiveness for non-fungible life-chance transfers.').

omega_variable(
    statelessness_as_structural_product,
    'Is statelessness an accidental gap in the sovereign system, or a structural product of a system that allocates rights exclusively through citizenship?',
    'Historical analysis of statelessness production: denationalization campaigns, state succession without citizenship provisions, birth registration gaps — are these bugs or features of the sovereign membership system?',
    'If structural product, the constraint''s victim set includes a permanently excluded class (stateless persons) whose exclusion is not incidental but systemic — reinforcing snare classification with identity-locked victims. If accidental, statelessness is a reparable administrative failure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(statelessness_as_structural_product, conceptual, 'Whether statelessness is a bug or feature of sovereign membership allocation.').

omega_variable(
    committer_kernel_relation,
    'How does the sovereignty reading''s structural relationship to the border_legitimacy kernel differ from its sibling readings, and what classification consequences follow?',
    'Structural comparison of the three readings'' beneficiary/victim sets, ε values, and enforcement logics. The engine''s kernel contamination analysis will trace how drift in one reading affects the others.',
    'Documents the committer-frame structural delta: sovereignty reading has high ε (0.78), excluded migrants as victims, state as legitimate enforcer. Freedom_of_movement_reading would have low ε, migrants as beneficiaries, borders as suspects. Humanitarian_obligation_reading would have medium ε, refugees as partial beneficiaries, economic migrants as victims. These are distinct constraints, not measurement variants.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_kernel_relation, conceptual, 'Commitment-system framing: this constraint as one reading of a contested kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_legitimacy__sovereignty_reading, 1948, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(border_sov_tr_t1948, border_legitimacy__sovereignty_reading, theater_ratio, 1948, 0.12).
narrative_ontology:measurement(border_sov_tr_t1965, border_legitimacy__sovereignty_reading, theater_ratio, 1965, 0.15).
narrative_ontology:measurement(border_sov_tr_t1980, border_legitimacy__sovereignty_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(border_sov_tr_t1995, border_legitimacy__sovereignty_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(border_sov_tr_t2005, border_legitimacy__sovereignty_reading, theater_ratio, 2005, 0.21).
narrative_ontology:measurement(border_sov_tr_t2015, border_legitimacy__sovereignty_reading, theater_ratio, 2015, 0.22).
narrative_ontology:measurement(border_sov_tr_t2025, border_legitimacy__sovereignty_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(border_sov_be_t1948, border_legitimacy__sovereignty_reading, base_extractiveness, 1948, 0.45).
narrative_ontology:measurement(border_sov_be_t1965, border_legitimacy__sovereignty_reading, base_extractiveness, 1965, 0.52).
narrative_ontology:measurement(border_sov_be_t1980, border_legitimacy__sovereignty_reading, base_extractiveness, 1980, 0.58).
narrative_ontology:measurement(border_sov_be_t1995, border_legitimacy__sovereignty_reading, base_extractiveness, 1995, 0.65).
narrative_ontology:measurement(border_sov_be_t2005, border_legitimacy__sovereignty_reading, base_extractiveness, 2005, 0.71).
narrative_ontology:measurement(border_sov_be_t2015, border_legitimacy__sovereignty_reading, base_extractiveness, 2015, 0.75).
narrative_ontology:measurement(border_sov_be_t2025, border_legitimacy__sovereignty_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(border_sov_su_t1948, border_legitimacy__sovereignty_reading, suppression_requirement, 1948, 0.55).
narrative_ontology:measurement(border_sov_su_t1965, border_legitimacy__sovereignty_reading, suppression_requirement, 1965, 0.62).
narrative_ontology:measurement(border_sov_su_t1980, border_legitimacy__sovereignty_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(border_sov_su_t1995, border_legitimacy__sovereignty_reading, suppression_requirement, 1995, 0.78).
narrative_ontology:measurement(border_sov_su_t2005, border_legitimacy__sovereignty_reading, suppression_requirement, 2005, 0.81).
narrative_ontology:measurement(border_sov_su_t2015, border_legitimacy__sovereignty_reading, suppression_requirement, 2015, 0.83).
narrative_ontology:measurement(border_sov_su_t2025, border_legitimacy__sovereignty_reading, suppression_requirement, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_legitimacy__sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, border_legitimacy__freedom_of_movement_reading).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, border_legitimacy__humanitarian_obligation_reading).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, refugee_convention_implementation).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, internal_enforcement_regime).
narrative_ontology:affects_constraint(border_legitimacy__sovereignty_reading, citizenship_allocation_rules).

% DUAL FORMULATION NOTE:
% Border legitimacy kernel decomposes into three constraint stories with distinct ε and victim sets. Sovereignty reading (this story) claims mountain-like naturalness but operates as snare with high extraction from excluded migrants. Freedom_of_movement_reading would claim rope but face enforcement suppression from sovereignty states. Humanitarian_obligation_reading claims scaffold (transitional protection) but has been captured by sovereign discretion. The three stories form a constraint family linked by mutual structural influence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_legitimacy__sovereignty_reading, institutional, 0.05).
constraint_indexing:directionality_override(border_legitimacy__sovereignty_reading, organized, 0.3).
constraint_indexing:directionality_override(border_legitimacy__sovereignty_reading, powerless, 0.98).
constraint_indexing:directionality_override(border_legitimacy__sovereignty_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
