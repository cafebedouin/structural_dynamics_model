% ============================================================================
% CONSTRAINT STORY: ost_article_ii_non_appropriation__commons_conservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ost_article_ii_non_appropriation__commons_conservation, []).

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
 *   constraint_id: ost_article_ii_non_appropriation__commons_conservation
 *   human_readable: OST Article II Non-Appropriation as Commons-Conservation Wall Constraint
 *   domain: international_law/space_governance/commons
 *
 * SUMMARY:
 *   This story instantiates the commons-conservation reading of Outer Space
 *   Treaty Article II's 'use or occupation' clause: that de facto
 *   appropriation via resource extraction is itself prohibited absent a
 *   multilateral authorization regime, and that the prohibition binds both
 *   states and private actors acting under state jurisdiction. Since the
 *   Artemis Accords and national space resource statutes (US 2015, Luxembourg
 *   2017, UAE, Japan) have proceeded on the contrary assumption that
 *   extraction and private ownership of extracted resources are compatible
 *   with Article II, this reading is increasingly a minority legal position
 *   defended chiefly by non-spacefaring states and a portion of the
 *   international law academy, rather than settled state practice. As
 *   commercial lunar and asteroid mining programs mature technically, the gap
 *   between this reading and actual state practice under the
 *   extraction_permissive reading widens, which is why extractiveness and
 *   suppression climb over the interval even though this reading's
 *   institutional enforcement capacity has not correspondingly grown.
 *
 * KEY AGENTS:
 *   - non_spacefaring_states: organized bloc with treaty-parity veto but no extraction capability — primary structural beneficiary of this reading
 *   - first_mover_mining_ventures: moderate power, trapped exit — bear stranded-asset risk if this reading is vindicated
 *   - spacefaring_states_with_extraction_capability: institutional power, constrained exit via treaty withdrawal costs — bear the practical prohibition
 *   - multilateral_treaty_framework: institutional/civilizational — the beneficiary institution whose relevance this reading sustains
 *   - international_law_scholars_and_tribunals: analytical observer seat interpreting the contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ost_article_ii_non_appropriation__commons_conservation, 0.42).
domain_priors:suppression_score(ost_article_ii_non_appropriation__commons_conservation, 0.55).
domain_priors:theater_ratio(ost_article_ii_non_appropriation__commons_conservation, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, extractiveness, 0.42).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(ost_article_ii_non_appropriation__commons_conservation, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ost_article_ii_non_appropriation__commons_conservation, tangled_rope).
narrative_ontology:human_readable(ost_article_ii_non_appropriation__commons_conservation, "OST Article II Non-Appropriation as Commons-Conservation Wall Constraint").
narrative_ontology:topic_domain(ost_article_ii_non_appropriation__commons_conservation, "international_law/space_governance/commons").

domain_priors:requires_active_enforcement(ost_article_ii_non_appropriation__commons_conservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ost_article_ii_non_appropriation__commons_conservation, '47dcba52-47b2-44e9-b306-d2828b7cc68d').
narrative_ontology:cs_kernel_codification('47dcba52-47b2-44e9-b306-d2828b7cc68d', fixed_text).
narrative_ontology:cs_authority_grounding('47dcba52-47b2-44e9-b306-d2828b7cc68d', distributed).
narrative_ontology:cs_reading_relation('47dcba52-47b2-44e9-b306-d2828b7cc68d', ost_article_ii_non_appropriation__extraction_permissive, forecloses).
narrative_ontology:cs_reading_relation('47dcba52-47b2-44e9-b306-d2828b7cc68d', ost_article_ii_non_appropriation__international_regime, influences).
narrative_ontology:cs_axiom('47dcba52-47b2-44e9-b306-d2828b7cc68d', foundational, extraction_constitutes_de_facto_appropriation).
narrative_ontology:cs_axiom_status(extraction_constitutes_de_facto_appropriation, holdable).
narrative_ontology:cs_axiom_grounding('47dcba52-47b2-44e9-b306-d2828b7cc68d', extraction_constitutes_de_facto_appropriation, conventional).
narrative_ontology:cs_axiom('47dcba52-47b2-44e9-b306-d2828b7cc68d', foundational, non_appropriation_binds_private_actors_via_state_jurisdiction).
narrative_ontology:cs_axiom_status(non_appropriation_binds_private_actors_via_state_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('47dcba52-47b2-44e9-b306-d2828b7cc68d', non_appropriation_binds_private_actors_via_state_jurisdiction, conventional).
narrative_ontology:cs_axiom('47dcba52-47b2-44e9-b306-d2828b7cc68d', secondary, capability_confers_no_extraction_entitlement).
narrative_ontology:cs_axiom_status(capability_confers_no_extraction_entitlement, holdable).
narrative_ontology:cs_axiom_grounding('47dcba52-47b2-44e9-b306-d2828b7cc68d', capability_confers_no_extraction_entitlement, deontological).
narrative_ontology:cs_reference_frame('47dcba52-47b2-44e9-b306-d2828b7cc68d', id_1967_drafting_era_anti_enclosure_consensus).
narrative_ontology:cs_drift_state('47dcba52-47b2-44e9-b306-d2828b7cc68d', commercial_space_resource_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('47dcba52-47b2-44e9-b306-d2828b7cc68d', '').
narrative_ontology:cs_kernel_id(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, future_generations_of_claimants).
narrative_ontology:constraint_beneficiary(ost_article_ii_non_appropriation__commons_conservation, multilateral_treaty_framework).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, first_mover_mining_ventures).
narrative_ontology:constraint_victim(ost_article_ii_non_appropriation__commons_conservation, spacefaring_states_with_extraction_capability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Lack independent launch or extraction capability but hold equal voting weight in treaty bodies and UN forums. Under this reading, their assent is required before any lawful extraction regime can be constituted, giving them an effective veto over enclosure they could never enforce unilaterally. They benefit from a rule that converts their diplomatic parity into a material claim on future off-world resources.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states, beneficiary,
    organized, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__commons_conservation, non_spacefaring_states, agenda_setter).

% Have sunk capital into prospecting, extraction technology, and mission planning premised on being able to sell or use what they extract. Under the commons-conservation reading, any actual removal and use of resources without prior multilateral authorization is itself the prohibited 'appropriation,' regardless of territorial claim — stranding their investment and exposing operations to legal challenge or exclusion from insurance, licensing, and downstream markets.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, first_mover_mining_ventures, payer,
    moderate, biographical, trapped, global).

% Possess the launch and extraction capability to act first but are treaty parties bound by Article II. Under this reading their capability confers no legal entitlement — extraction absent a multilateral authorization regime is de facto appropriation. They can lobby to renegotiate or exit the treaty, but withdrawal carries severe diplomatic and reciprocity costs, so exit is constrained rather than open.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, spacefaring_states_with_extraction_capability, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__commons_conservation, spacefaring_states_with_extraction_capability, agenda_setter).

% The negotiated international regime (or its future incarnation) that would authorize and allocate extraction rights. Its legitimacy and relevance are sustained precisely by this reading, which routes all lawful appropriation through it rather than through unilateral capability — the framework's continued negotiating leverage depends on extraction remaining prohibited until it acts.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, multilateral_treaty_framework, beneficiary,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_secondary_role(ost_article_ii_non_appropriation__commons_conservation, multilateral_treaty_framework, observer).

% Firms licensed under domestic space resource statutes (which assert private ownership of extracted resources is compatible with Article II) are not party to treaty negotiations and have no formal voice in the international interpretive contest, yet their commercial viability depends entirely on which reading prevails.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, private_commercial_actors_domestic_licensees, excluded,
    moderate, immediate, trapped, national).

% Interpret the 'use or occupation' clause's drafting history, subsequent state practice, and customary international law to adjudicate between competing readings; their scholarship and any future tribunal rulings would determine which reading, if any, becomes authoritative practice.
narrative_ontology:constraint_stakeholder(ost_article_ii_non_appropriation__commons_conservation, international_law_scholars_and_tribunals, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ost_article_ii_non_appropriation__commons_conservation, diffuse).
narrative_ontology:fixing_cost_class(ost_article_ii_non_appropriation__commons_conservation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents a race-to-extract dynamic in which whichever state or firm reaches an asteroid or lunar deposit first locks in de facto control, by requiring that any lawful extraction be authorized through a collectively negotiated regime rather than unilateral capability.
% TRANSFER_FUNCTION: Moves the practical option to extract and monetize space resources away from technologically capable first movers and toward a negotiated allocation that non-spacefaring states can shape, converting diplomatic parity into a claim on future resource value; correspondingly moves sunk investment risk onto capable actors who act before authorization exists.
% ABSENT_VOICES: Domestic private licensees operating under national space resource acts (which proceed as if extraction is already lawful) have no seat in the treaty interpretation contest; asteroid-mining investors and their financiers are likewise structurally absent from the diplomatic forums where this reading is asserted and defended.
% DISAPPEARANCE_RATIONALE: If this reading were abandoned overnight — that is, if the international consensus shifted decisively to the extraction-permissive reading — first-mover ventures would proceed immediately, national licensing regimes would gain legal cover, non-spacefaring states would lose their negotiating leverage over resource allocation, and the informal moratorium on unilateral extraction that currently constrains capable actors would end.
% FOUNDING_PROBLEM: The 1967 drafters sought to prevent Cold War-era territorial competition from extending into space, and to preserve outer space (including celestial bodies) as a domain not subject to national appropriation by any means — the commons-conservation reading extends that founding concern to resource extraction, treating de facto appropriation through extraction as functionally equivalent to the territorial claims Article II explicitly bars.
% FOUNDING_PROBLEM_CORROBORATION: Non-spacefaring states and UN COPUOS delegations from the Global South attest the founding concern (preventing capability-based enclosure) remains live and applies squarely to resource extraction. Independent international law scholars outside both the extraction industry and the non-spacefaring bloc note the 1967 drafters did not anticipate commercial asteroid mining and that the text is genuinely silent on extracted-resource ownership — meaning the commons-conservation reading is a defensible but not compelled extension, not a settled original meaning.
narrative_ontology:disappearance_verdict(ost_article_ii_non_appropriation__commons_conservation, world_rearranges).
narrative_ontology:founding_problem_status(ost_article_ii_non_appropriation__commons_conservation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ost_article_ii_non_appropriation__commons_conservation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(ost_article_ii_non_appropriation__commons_conservation, 'none', 1).
narrative_ontology:epsilon_provenance(ost_article_ii_non_appropriation__commons_conservation, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ost_article_ii_non_appropriation__commons_conservation_tests).
:- end_tests(ost_article_ii_non_appropriation__commons_conservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate (0.42) rather than high because this reading, unlike a captured extraction regime, imposes cost primarily through prohibition and stranded investment rather than through active rent collection by an identifiable extractor — no party here collects what first movers lose; the loss is diffuse and structural, which is why gain_flow is authored as diffuse rather than naming a beneficiary seat. Suppression is authored at a meaningfully high level (0.55) because enforcing a de facto appropriation prohibition against a technologically capable, well-resourced actor requires real diplomatic and legal coercion (treaty complaint mechanisms, denial of insurance/licensing, reputational sanction) — this is not a costless norm. Resistance is high (0.72) because spacefaring states and their commercial sectors actively contest this reading through counter-legislation (national space resource acts) rather than passively complying. Accessibility collapse is moderate (0.6): alternative readings remain legally live and are being actively pursued through domestic legislation, so alternatives have not collapsed the way they would under a genuine mountain.
 *
 * PERSPECTIVAL GAP:
 *   From the non_spacefaring_states seat, this reading is coordination: it prevents a scramble that would permanently exclude states without launch capability from ever sharing in space resource wealth. From the first_mover_mining_ventures and capable-state seats, the identical textual claim operates as pure extraction of their sunk capital and technological lead — capability that took decades and billions to build is nullified by a legal veto held by parties who contributed none of it. The engine should compute these as structurally different experiences of one constraint, not reconcile them.
 *
 * DIRECTIONALITY LOGIC:
 *   Non-spacefaring states and the multilateral framework are coded as beneficiaries: the non-appropriation-covers-extraction reading is what gives their treaty vote real economic weight, converting a diplomatic asset into a material one. First-mover ventures and capable spacefaring states are coded as victims/payers: their capability is precisely what the reading nullifies unless routed through a negotiated regime they do not control. Exit options differentiate importantly — capable states hold constrained exit (treaty withdrawal is possible but diplomatically costly), while private ventures hold trapped exit (they cannot withdraw from international law the way a state can withdraw from a treaty; they are bound by whichever national licensing framework and international consensus prevails).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing capability-based territorial enclosure) is contested as either fully live (non-spacefaring states' view) or substantially transformed by 60 years of non-appropriation of actual territory alongside emerging technological capability the drafters never anticipated (the scholarly middle view). This reading is not classified as mandatrophy-resolved because the underlying coordination problem — avoiding a resource grab that permanently disadvantages non-capable parties — remains a live, contestable policy question rather than a settled non-issue; classifying it as tangled_rope rather than snare or pure rope preserves the coordination function (avoiding enclosure races) while registering the real cost imposed on capable actors, rather than collapsing the analysis into either pure legitimate coordination or pure capture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_scope_of_use_or_occupation,
    'Does Article II''s phrase ''national appropriation by claim of sovereignty, by means of use or occupation, or by any other means'' extend to resource extraction and consumption, or is it limited to territorial/sovereignty claims over celestial bodies themselves?',
    'Authoritative interpretation by the International Court of Justice, a binding arbitral panel, or overwhelming, unambiguous subsequent state practice (opinio juris) converging on one reading; absent that, the ambiguity persists as a live legal question.',
    'If the extraction-permissive reading is vindicated, this constraint''s entire prohibitive function dissolves and first-mover ventures bear no legal risk; if the commons-conservation reading is vindicated, existing national space resource statutes become internationally unlawful and require renegotiation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(textual_scope_of_use_or_occupation, conceptual, 'Core textual/interpretive ambiguity of the non-appropriation clause''s scope.').

omega_variable(
    kernel_reading_identity,
    'This story instantiates the commons_conservation reading of the ost_article_ii_non_appropriation kernel. The sibling readings — extraction_permissive (private ownership of extracted resources is compatible with the sovereignty-claim bar) and international_regime (the question is deferred to a future Article XI-analogue multilateral framework) — are separate constraints with their own ε values, not alternative measurements of this one.',
    'Track state practice, domestic legislation, and any future multilateral resource regime (e.g. a COPUOS working group outcome) to see which reading state and commercial practice actually converges on over time.',
    'Convergence toward extraction_permissive would mean this reading''s stakeholders'' situations (stranded investment, treaty-veto leverage) never materialize as described; convergence toward international_regime would mean both this reading and extraction_permissive are superseded by a negotiated allocation framework neither fully anticipates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Committer-frame declaration: this is one reading of a three-way contested kernel; the readings are siblings, not measurements.').

omega_variable(
    enforceability_absent_hegemon_compliance,
    'Can this reading''s prohibition be meaningfully enforced against a major spacefaring power that has enacted contrary domestic legislation and shows no intent to comply, given the absence of a space-law enforcement body with binding jurisdiction?',
    'Observe whether any international body, coalition, or reciprocal-sanction mechanism successfully constrains an extraction attempt by a non-compliant capable state or its licensed private actor.',
    'If unenforceable in practice, the constraint''s suppression score is more theatrical than the measurements suggest and the story drifts toward piton (prohibition maintained rhetorically while extraction proceeds); if enforcement mechanisms emerge (e.g. denial of launch cooperation, market access, or insurance), the tangled_rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforceability_absent_hegemon_compliance, empirical, 'Whether the non-appropriation prohibition has teeth absent a supranational enforcement body.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ost_article_ii_non_appropriation__commons_conservation, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ost__tr_t0, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ost__tr_t8, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 8, 0.18).
narrative_ontology:measurement(ost__tr_t16, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 16, 0.22).
narrative_ontology:measurement(ost__tr_t24, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 24, 0.26).
narrative_ontology:measurement(ost__tr_t32, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 32, 0.28).
narrative_ontology:measurement(ost__tr_t40, ost_article_ii_non_appropriation__commons_conservation, theater_ratio, 40, 0.3).

% Extraction over time
narrative_ontology:measurement(ost__be_t0, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(ost__be_t8, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 8, 0.25).
narrative_ontology:measurement(ost__be_t16, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 16, 0.32).
narrative_ontology:measurement(ost__be_t24, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 24, 0.38).
narrative_ontology:measurement(ost__be_t32, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 32, 0.4).
narrative_ontology:measurement(ost__be_t40, ost_article_ii_non_appropriation__commons_conservation, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(ost__su_t0, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(ost__su_t8, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 8, 0.4).
narrative_ontology:measurement(ost__su_t16, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 16, 0.46).
narrative_ontology:measurement(ost__su_t24, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(ost__su_t32, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 32, 0.53).
narrative_ontology:measurement(ost__su_t40, ost_article_ii_non_appropriation__commons_conservation, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ost_article_ii_non_appropriation__commons_conservation, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(ost_article_ii_non_appropriation__commons_conservation, 0.12).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation__extraction_permissive).
narrative_ontology:affects_constraint(ost_article_ii_non_appropriation__commons_conservation, ost_article_ii_non_appropriation__international_regime).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the ost_article_ii_non_appropriation kernel, decomposed per the epsilon-invariance principle because the three readings assign structurally different beneficiary/victim sets and different epsilon values to the same treaty text: commons_conservation (this story, moderate-high epsilon, tangled_rope, non-spacefaring states as beneficiaries) sees extraction itself as the prohibited act; extraction_permissive (lower epsilon, closer to rope, spacefaring commercial actors as beneficiaries) sees only sovereignty claims as prohibited, leaving private resource ownership lawful; international_regime (distinct epsilon, closer to scaffold pending a negotiated framework) treats the question as unresolved and deferred to a future multilateral body. All three must be read together to understand the actual contested legal terrain; none alone is 'the' Article II constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
