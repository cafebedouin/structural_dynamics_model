% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__expansive_humanitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__expansive_humanitarian_reading, []).

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
 *   constraint_id: refugee_convention_text__expansive_humanitarian_reading
 *   human_readable: Refugee Convention — Expansive Humanitarian Reading
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   This constraint story captures the expansive humanitarian reading of the
 *   1951 Refugee Convention and its 1967 Protocol — the reading that treats
 *   the Convention as an unbendable humanitarian mandate requiring broad
 *   protection. It extends 'well-founded fear' to generalized violence and
 *   non-state persecution, and reads 'particular social group' to encompass
 *   gender, LGBTQ+, and clan-based persecution. It treats interdiction,
 *   offshore processing, and safe third country agreements as refoulement
 *   violations. It imposes a duty to substantively assess all claims. This is
 *   one of three contested readings of the refugee_convention_text kernel.
 *
 * KEY AGENTS:
 *   - asylum_seekers_generalized_violence: Primary beneficiary (powerless/trapped) — receives protection from return to war zones
 *   - asylum_seekers_non_state_persecution: Primary beneficiary (powerless/trapped) — receives protection from non-state actors
 *   - asylum_seekers_social_group_gender: Primary beneficiary (powerless/identity_locked) — gender recognized as particular social group
 *   - asylum_seekers_social_group_lgbtq: Primary beneficiary (powerless/identity_locked) — LGBTQ+ recognized as particular social group
 *   - asylum_seekers_social_group_clan: Primary beneficiary (powerless/identity_locked) — clan membership recognized as particular social group
 *   - unhcr_advocacy_network: Agenda setter (institutional/arbitrage) — drives expansive interpretation, benefits from broad mandate
 *   - human_rights_ngos: Agenda setter (organized/mobile) — litigates and advocates for expansive reading
 *   - destination_states_frontline: Primary payer (institutional/constrained) — bears disproportionate reception costs
 *   - destination_states_interior: Primary payer (institutional/constrained) — bears integration costs and political backlash
 *   - asylum_administration_systems: Payer (organized/constrained) — bears administrative extraction of complex caseloads
 *   - border_management_agencies: Payer (organized/constrained) — bears operational extraction of compliance demands
 *   - restrictive_interpretation_states: Excluded (institutional/mobile) — advocates restrictive reading, objects to expansive obligations
 *   - international_legal_scholars: Observer (analytical/analytical) — maps the interpretive field
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__expansive_humanitarian_reading, 0.68).
domain_priors:suppression_score(refugee_convention_text__expansive_humanitarian_reading, 0.42).
domain_priors:theater_ratio(refugee_convention_text__expansive_humanitarian_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__expansive_humanitarian_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__expansive_humanitarian_reading, "Refugee Convention — Expansive Humanitarian Reading").
narrative_ontology:topic_domain(refugee_convention_text__expansive_humanitarian_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__expansive_humanitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__expansive_humanitarian_reading, '443bd8ba-4c0f-4daf-a63a-ba51424285e4').
narrative_ontology:cs_kernel_codification('443bd8ba-4c0f-4daf-a63a-ba51424285e4', formalized).
narrative_ontology:cs_authority_grounding('443bd8ba-4c0f-4daf-a63a-ba51424285e4', lineage).
narrative_ontology:cs_interpretation_layer_present('443bd8ba-4c0f-4daf-a63a-ba51424285e4').
narrative_ontology:cs_reading_relation('443bd8ba-4c0f-4daf-a63a-ba51424285e4', refugee_convention_text__restrictive_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('443bd8ba-4c0f-4daf-a63a-ba51424285e4', refugee_convention_text__procedural_integrity_reading, influences).
narrative_ontology:cs_axiom('443bd8ba-4c0f-4daf-a63a-ba51424285e4', foundational, humanitarian_obligation_primacy_over_sovereignty).
narrative_ontology:cs_axiom_status(humanitarian_obligation_primacy_over_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('443bd8ba-4c0f-4daf-a63a-ba51424285e4', humanitarian_obligation_primacy_over_sovereignty, deontological).
narrative_ontology:cs_axiom('443bd8ba-4c0f-4daf-a63a-ba51424285e4', foundational, non_refoulement_absolute_no_exceptions).
narrative_ontology:cs_axiom_status(non_refoulement_absolute_no_exceptions, holdable).
narrative_ontology:cs_axiom_grounding('443bd8ba-4c0f-4daf-a63a-ba51424285e4', non_refoulement_absolute_no_exceptions, deontological).
narrative_ontology:cs_axiom('443bd8ba-4c0f-4daf-a63a-ba51424285e4', foundational, particular_social_group_includes_gender_lgbtq_clan).
narrative_ontology:cs_axiom_status(particular_social_group_includes_gender_lgbtq_clan, holdable).
narrative_ontology:cs_axiom_grounding('443bd8ba-4c0f-4daf-a63a-ba51424285e4', particular_social_group_includes_gender_lgbtq_clan, empirically_contingent).
narrative_ontology:cs_axiom('443bd8ba-4c0f-4daf-a63a-ba51424285e4', foundational, well_founded_fear_includes_generalized_violence).
narrative_ontology:cs_axiom_status(well_founded_fear_includes_generalized_violence, holdable).
narrative_ontology:cs_axiom_grounding('443bd8ba-4c0f-4daf-a63a-ba51424285e4', well_founded_fear_includes_generalized_violence, empirically_contingent).
narrative_ontology:cs_axiom('443bd8ba-4c0f-4daf-a63a-ba51424285e4', foundational, non_state_actors_can_be_persecutors).
narrative_ontology:cs_axiom_status(non_state_actors_can_be_persecutors, holdable).
narrative_ontology:cs_axiom_grounding('443bd8ba-4c0f-4daf-a63a-ba51424285e4', non_state_actors_can_be_persecutors, empirically_contingent).
narrative_ontology:cs_reference_frame('443bd8ba-4c0f-4daf-a63a-ba51424285e4', post_wwii_refugee_protection_framework).
narrative_ontology:cs_drift_state('443bd8ba-4c0f-4daf-a63a-ba51424285e4', contemporary_mixed_migration_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('443bd8ba-4c0f-4daf-a63a-ba51424285e4', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_generalized_violence).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_non_state_persecution).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_social_group_gender).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_social_group_lgbtq).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_social_group_clan).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, unhcr_advocacy_network).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, human_rights_ngos).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, destination_states_frontline).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, destination_states_interior).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, asylum_administration_systems).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, border_management_agencies).
narrative_ontology:constraint_vindicates(refugee_convention_text__expansive_humanitarian_reading, humanitarian_obligation_primacy).
narrative_ontology:constraint_vindicates(refugee_convention_text__expansive_humanitarian_reading, non_refoulement_absolute).
narrative_ontology:constraint_vindicates(refugee_convention_text__expansive_humanitarian_reading, particular_social_group_expansive).
narrative_ontology:constraint_vindicates(refugee_convention_text__expansive_humanitarian_reading, well_founded_fear_contextual).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Flee war zones, collapsed states, and generalized violence where no individual persecution can be proven but return means likely death. Under this reading, their fear is well-founded without needing to show individualized targeting. They have no exit options — they are the constraint's primary intended beneficiaries.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_generalized_violence, beneficiary,
    powerless, biographical, trapped, global).

% Face persecution from armed groups, cartels, clans, families, or religious militias where the state is unwilling or unable to protect. This reading treats non-state actors as persecutors equivalent to states. They cannot access protection in their home country and have no alternative but to seek asylum abroad.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_non_state_persecution, beneficiary,
    powerless, biographical, trapped, global).

% Women and girls fleeing gender-based violence — honor killings, forced marriage, FGM, domestic violence as systemic persecution. Their membership in the social group 'women' is immutable and identity-locked; exit from the persecution requires physical escape and recognition of gender as a particular social group.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_social_group_gender, beneficiary,
    powerless, biographical, identity_locked, global).

% LGBTQ+ individuals facing criminalization, violence, or death due to sexual orientation or gender identity. Their social group membership is immutable and identity-locked; the persecution is often state-sanctioned or state-tolerated. Recognition of LGBTQ+ as a particular social group is essential for their protection.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_social_group_lgbtq, beneficiary,
    powerless, biographical, identity_locked, global).

% Members of clans, tribes, or kinship groups targeted in clan-based conflicts or blood feuds where the persecutor is the clan structure itself. Membership is ascribed at birth and identity-locked; escape requires both physical flight and recognition that clan membership constitutes a particular social group.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_social_group_clan, beneficiary,
    powerless, biographical, identity_locked, global).

% UNHCR and its partner NGOs drive the expansive interpretation through guidelines, interventions, and supervisory mechanisms. They benefit institutionally from a broad mandate that justifies their operational scope, funding, and authority. They can shift focus across regions and issues — arbitrage-grade exit from any single state's resistance.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, unhcr_advocacy_network, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__expansive_humanitarian_reading, unhcr_advocacy_network, beneficiary).

% International and domestic human rights organizations litigate, advocate, and monitor for expansive protection. They benefit from the legal tools this reading provides. Their organizational survival does not depend on any single jurisdiction's compliance — mobile exit options across forums and jurisdictions.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, human_rights_ngos, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__expansive_humanitarian_reading, human_rights_ngos, beneficiary).

% States of first arrival (Greece, Italy, Turkey, Bangladesh, Kenya, etc.) bear disproportionate reception, processing, and integration costs under this reading's broad eligibility. They cannot easily exit the constraint — geography and non-refoulement bind them. Constrained exit: they can push for burden-sharing but cannot unilaterally opt out.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, destination_states_frontline, payer,
    institutional, biographical, constrained, regional).

% Destination states further from borders (Germany, Sweden, Canada, US, Australia) bear integration costs and political backlash. They have more capacity but face domestic political constraints. Constrained exit: they can restrict access through interdiction, offshore processing, or safe third country agreements — but this reading treats those as refoulement violations.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, destination_states_interior, payer,
    institutional, biographical, constrained, regional).

% National asylum offices, courts, and appeal bodies face exponentially growing caseloads with complex claims (social group analysis, non-state persecution, generalized violence assessment). They bear the administrative extraction. Constrained exit: they can request resources or reform but cannot refuse to process claims.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, asylum_administration_systems, payer,
    organized, biographical, constrained, national).

% Coast guards, border police, and immigration enforcement tasked with interdiction and pushbacks that this reading classifies as refoulement violations. They bear the operational extraction of compliance demands. Constrained exit: they follow state policy; non-compliance risks prosecution.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, border_management_agencies, payer,
    organized, immediate, constrained, national).

% States advocating the restrictive sovereignty reading (e.g., Hungary, Poland, Australia, US at times) — they would object to the expansive reading's obligations but are bound by the Convention's text and UNHCR's supervisory role. They exit by non-compliance, withdrawal threats, or bilateral deals — mobile across policy options.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, restrictive_interpretation_states, excluded,
    institutional, generational, mobile, national).

% Academics and jurists analyzing the Convention's interpretation, tracking state practice, and mapping the divergence between the three readings. They neither collect nor pay — they map the structural field.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal, non-discriminatory framework for protecting those whose states have failed them — solving the coordination problem of who bears responsibility for the displaced when the state of origin has collapsed or turned persecutor, and preventing a race to the bottom where states compete to exclude.
% TRANSFER_FUNCTION: Moves protection obligations, reception costs, integration burdens, and administrative load from the persecuted (who have nothing) to destination states (who have capacity), mediated by the UNHCR supervisory system and the non-refoulement principle as the transfer mechanism.
% ABSENT_VOICES: Internally displaced persons who never cross borders (the Convention only covers those outside their country); stateless persons without a clear persecutor; would-be migrants who don't meet the refugee definition but face existential precarity; states of origin that produce the displacement but bear no cost under the Convention.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, millions currently eligible for protection would lose legal status; frontline states would close borders and push back boats without legal consequence; UNHCR's mandate would shrink to individualized persecution only; the global protection regime would collapse into bilateral ad hoc arrangements or nothing.
% FOUNDING_PROBLEM: The post-WWII refugee crisis revealed that states could denationalize, expel, and murder their own citizens with impunity, and the international system had no mechanism to intervene. The Convention was built to solve: how to obligate states to protect people their own state has abandoned or targeted, without requiring the persecuting state's consent.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the Convention's travaux préparatoires and the UNHCR Statute — sources outside the current beneficiary set. However, restrictive_sovereignty_reading proponents argue the founding problem was only individualized state persecution, not generalized violence or non-state actors; procedural_integrity_reading proponents argue the problem was procedural vacuum, not substantive outcome. The corroboration is contested across the kernel's readings.
narrative_ontology:disappearance_verdict(refugee_convention_text__expansive_humanitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__expansive_humanitarian_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__expansive_humanitarian_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(refugee_convention_text__expansive_humanitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__expansive_humanitarian_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__expansive_humanitarian_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(refugee_convention_text__expansive_humanitarian_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(refugee_convention_text__expansive_humanitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) is substantial: the reading transfers massive, open-ended obligations to destination states — reception, processing, integration, non-refoulement compliance — with no cap and expanding eligibility. Suppression (0.42) is moderate: the constraint operates through legal obligation and supervisory pressure, not direct coercion; states resist through non-compliance, but the legal architecture suppresses exit options (withdrawal is costly, non-compliance risks condemnation). Theater ratio (0.28) reflects genuine protection function mixed with performative compliance — states sign, ratify, and maintain asylum systems but increasingly evade through interdiction and deterrence. Accessibility collapse (0.35) is moderate: alternatives exist (bilateral deals, regional arrangements, complementary protection) but the Convention remains the primary legal frame. Resistance (0.55) is high: frontline states, interior states, and restrictive states actively resist through policy, litigation, and structural non-compliance. The claimed type is tangled_rope because there is a genuine coordination function (preventing protection gaps, race-to-the-bottom) AND asymmetric extraction (destination states pay, asylum seekers benefit, UNHCR/NGOs benefit institutionally) AND active enforcement (UNHCR supervision, court rulings, treaty body pressure).
 *
 * PERSPECTIVAL GAP:
 *   From the beneficiary/agenda-setter seats (asylum seekers, UNHCR, NGOs), the constraint appears as a rope or even mountain — genuine coordination solving a collective action problem of protection. From the payer seats (destination states, administrations), it appears as a snare — extraction without consent, expanding eligibility, no exit. The engine computes this per-seat divergence from the power/exit/role structure authored here. The expansive reading's broadening of eligibility over time (measurements show rising extractiveness) means the payer seats experience increasing χ while beneficiary seats experience stable or increasing subsidy.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (asylum seekers across all categories, UNHCR, NGOs) are structural beneficiaries — the constraint subsidizes their protection claims and institutional mandates. Their directionality d is near 0.0 (full beneficiary). Victims (destination states frontline and interior, asylum administrations, border agencies) are structural targets — they bear the costs of reception, processing, integration, and compliance. Their directionality d is near 1.0 (full target), modulated by power (institutional/organized) and exit options (constrained — they cannot easily exit the treaty regime). Excluded actors (restrictive states) have mobile exit — they can non-comply or withdraw. Observers (scholars) have analytical exit. The engine derives d from these structural declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (state persecution of own citizens) is contested as live/dead — state persecution persists but the dominant displacement drivers are now generalized violence and non-state actors, which the founding text did not clearly contemplate. The reading resolves mandatrophy by expanding the mandate to cover the actual displacement drivers, preventing the constraint from becoming a piton (vestigial, protecting only a shrinking category of 'Convention refugees'). However, this expansion increases extractiveness on payer seats, creating the tangled_rope tension. The mandate has not atrophied — it has mutated to stay live — but the mutation transfers costs to parties who did not consent to the expanded scope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the expansive_humanitarian_reading a structurally distinct constraint from the restrictive_sovereignty_reading and procedural_integrity_reading, or are they observably the same constraint measured differently?',
    'Apply the ε-invariance test: if measuring the Convention''s operation through the expansive reading''s eligibility criteria yields ε ≈ 0.68 while the restrictive reading''s criteria yields ε ≈ 0.25, they are different constraints. The test is whether the beneficiary/victim sets, enforcement mechanisms, and extraction flows differ structurally.',
    'If they are one constraint, the corpus should have one story with measurement-basis parameters. If they are three constraints (as authored), each gets its own ε, stakeholders, and classification, linked by network.affects_constraints. The current authoring assumes three constraints per DP-001.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel''s readings instantiate separate constraints per ε-invariance principle.').

omega_variable(
    expansive_reading_forecloses_restrictive,
    'Does the expansive reading''s core premise (humanitarian mandate requires broad protection) logically foreclose the restrictive reading''s core premise (sovereign discretion is primary) within a single legal framework?',
    'Analyze whether a state or court could simultaneously hold that the Convention is an unbendable humanitarian mandate AND that it permits maximum sovereign discretion. If the premises are logically contradictory in a single framework, the relation is forecloses; if different actors can hold each simultaneously in different forums, it is coexists_with.',
    'Determines cs_structure.reading_relations: forecloses vs coexists_with. If forecloses, the engine may compute axiom contradiction for empirically_contingent axioms under drift. If coexists_with, both remain live in the kernel''s dispute space.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expansive_reading_forecloses_restrictive, conceptual, 'Structural relationship between expansive and restrictive readings within a single commitment framework.').

omega_variable(
    expansive_reading_influences_procedural,
    'Does the expansive reading create structural downstream pressure on the procedural_integrity reading (changing legitimacy conditions, resource availability) without foreclosing it?',
    'Track whether expansive reading''s broad eligibility (more claims, more complex claims) strains asylum procedures, forcing procedural_integrity advocates to either accept degraded process or restrict eligibility — i.e., whether the expansive reading''s resource demands structurally pressure the procedural reading''s operating conditions.',
    'If yes, relation is influences. This captures the real dynamic where expansive substantive claims overload the procedural safeguards the procedural reading treats as non-negotiable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(expansive_reading_influences_procedural, empirical, 'Downstream structural pressure from expansive eligibility on procedural integrity.').

omega_variable(
    social_group_coherence,
    'Is ''particular social group'' as expanded to gender, LGBTQ+, and clan a coherent legal category, or does the expansion dissolve the category''s analytic integrity?',
    'Track jurisprudential coherence: if courts apply fundamentally different tests for gender vs LGBTQ+ vs clan claims, the category may be a nominal unity masking structural fragmentation. If a unified ''immutable characteristic / fundamental identity / social visibility'' test emerges, coherence holds.',
    'If the category fragments, the expansive reading may be multiple constraints (gender_persecution, lgbtq_persecution, clan_persecution) linked by network, not one constraint. This would change ε for each and alter the tangled_rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(social_group_coherence, conceptual, 'Whether the expansive social group category is one constraint or a family.').

omega_variable(
    generalized_violence_extraction_boundary,
    'Where does the coordination function of protecting generalized violence flee end and pure extraction on destination states begin?',
    'Identify the threshold where the protection obligation exceeds the capacity of the international system to distribute burden equitably. If frontline states bear >80% of costs with <20% of global GDP, the coordination function has likely collapsed into extraction.',
    'If the boundary has been crossed, the constraint may be drifting from tangled_rope toward snare for frontline states. The measurement series (rising extractiveness, stable suppression) suggests this drift is underway.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(generalized_violence_extraction_boundary, preference, 'Whether the generalized violence protection obligation has exceeded equitable burden-sharing capacity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__expansive_humanitarian_reading, 1951, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_tr_t1951, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 1951, 0.05).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_tr_t1967, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 1967, 0.07).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_tr_t1980, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_tr_t1990, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_tr_t2000, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_tr_t2010, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_tr_t2020, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 2020, 0.27).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_tr_t2025, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_be_t1951, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 1951, 0.15).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_be_t1967, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 1967, 0.18).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_be_t1980, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 1980, 0.25).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_be_t1990, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 1990, 0.38).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_be_t2000, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_be_t2010, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 2010, 0.58).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_be_t2020, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 2020, 0.65).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_be_t2025, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_su_t1951, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 1951, 0.15).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_su_t1967, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 1967, 0.18).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_su_t1980, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 1980, 0.25).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_su_t1990, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 1990, 0.32).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_su_t2000, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 2000, 0.38).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_su_t2010, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_su_t2020, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 2020, 0.41).
narrative_ontology:measurement(refugee_convention_text__expansive_humanitarian_reading_su_t2025, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 2025, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__expansive_humanitarian_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(refugee_convention_text__expansive_humanitarian_reading, 0.12).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text__restrictive_sovereignty_reading).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text__procedural_integrity_reading).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, eu_asylum_acquis).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, global_compact_on_refugees).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, non_refoulement_customary_law).

% DUAL FORMULATION NOTE:
% This constraint is the expansive_humanitarian_reading of the refugee_convention_text kernel. It differs from the restrictive_sovereignty_reading in beneficiary/victim structure (broad vs narrow), extractiveness (0.68 vs ~0.25), and suppression mechanism (legal obligation vs sovereign discretion). It differs from the procedural_integrity_reading in prioritizing substantive outcome over process. All three share the same kernel text but instantiate different constraints per ε-invariance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(refugee_convention_text__expansive_humanitarian_reading, institutional, 0.85).
constraint_indexing:directionality_override(refugee_convention_text__expansive_humanitarian_reading, organized, 0.75).
constraint_indexing:directionality_override(refugee_convention_text__expansive_humanitarian_reading, powerless, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
