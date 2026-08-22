% ============================================================================
% CONSTRAINT STORY: border_normative_status__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   human_readable: Sovereignty-Primary Border Legitimacy (Exclusionary Reading)
 *   domain: political_philosophy/international_law/migration
 *
 * SUMMARY:
 *   This constraint story instantiates the sovereignty-primary reading of the
 *   contested border-legitimacy kernel. It describes the normative claim that
 *   territorial boundaries are legitimate instruments of collective
 *   self-determination and that states have foundational authority to exclude
 *   non-members. Under this reading, the border is not a coercive extraction
 *   mechanism but the constitutive boundary that makes collective political
 *   identity and democratic self-governance possible. Excluded migrants and
 *   displaced populations enter the victim set because they bear the costs of
 *   enforcement and boundary maintenance without voice in the democratic
 *   process that legitimates the arrangement. The constraint is CLAIMED as
 *   tangled_rope (genuine coordination function for member polity +
 *   asymmetric extraction from excluded populations) and the authored metrics
 *   describe substantially extractive, actively enforced operation with
 *   rising theater ratio over 80 years, indicating increasing performative
 *   maintenance relative to functional coordination. The reading acknowledges
 *   that this constraint is one position in a three-way kernel contest; the
 *   sibling readings (freedom_primary, qualified_sovereignty) are OTHER
 *   constraints in the corpus, not alternative interpretations of this one.
 *
 * KEY AGENTS:
 *   - citizen_polity: collective beneficiary and co-agenda-setter; claims self-determination authority
 *   - state_institutional_apparatus: administers and enforces exclusion; executes the polity's self-determination claim
 *   - excluded_migrants: primary victim; powerless, trapped at borders or in irregular status; bear enforcement costs without consent
 *   - internal_displacement_populations: secondary victim; formally members but displaced by collective priority-setting; lack veto power
 *   - neighboring_polities: observers that validate the framework through reciprocal practice
 *   - human_rights_advocacy_bodies: excluded from the legitimacy framework; would contest the arrangement but are categorized as external imposers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__sovereignty_primary, 0.68).
domain_priors:suppression_score(border_normative_status__sovereignty_primary, 0.71).
domain_priors:theater_ratio(border_normative_status__sovereignty_primary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, extractiveness, 0.68).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, accessibility_collapse, 0.73).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(border_normative_status__sovereignty_primary, "Sovereignty-Primary Border Legitimacy (Exclusionary Reading)").
narrative_ontology:topic_domain(border_normative_status__sovereignty_primary, "political_philosophy/international_law/migration").

domain_priors:requires_active_enforcement(border_normative_status__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__sovereignty_primary, '25687dcc-f2df-4ba6-a0a4-42407ccf224c').
narrative_ontology:cs_kernel_codification('25687dcc-f2df-4ba6-a0a4-42407ccf224c', distributed).
narrative_ontology:cs_authority_grounding('25687dcc-f2df-4ba6-a0a4-42407ccf224c', lineage).
narrative_ontology:cs_interpretation_layer_present('25687dcc-f2df-4ba6-a0a4-42407ccf224c').
narrative_ontology:cs_reading_relation('25687dcc-f2df-4ba6-a0a4-42407ccf224c', border_normative_status__freedom_primary, coexists_with).
narrative_ontology:cs_reading_relation('25687dcc-f2df-4ba6-a0a4-42407ccf224c', border_normative_status__qualified_sovereignty, influences).
narrative_ontology:cs_axiom('25687dcc-f2df-4ba6-a0a4-42407ccf224c', foundational, territorial_self_determination_doctrine).
narrative_ontology:cs_axiom_status(territorial_self_determination_doctrine, holdable).
narrative_ontology:cs_axiom_grounding('25687dcc-f2df-4ba6-a0a4-42407ccf224c', territorial_self_determination_doctrine, deontological).
narrative_ontology:cs_axiom('25687dcc-f2df-4ba6-a0a4-42407ccf224c', secondary, state_sovereignty_supremacy).
narrative_ontology:cs_axiom_status(state_sovereignty_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('25687dcc-f2df-4ba6-a0a4-42407ccf224c', state_sovereignty_supremacy, deontological).
narrative_ontology:cs_reference_frame('25687dcc-f2df-4ba6-a0a4-42407ccf224c', westphalian_territorial_sovereignty).
narrative_ontology:cs_drift_state('25687dcc-f2df-4ba6-a0a4-42407ccf224c', contemporary_2025, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('25687dcc-f2df-4ba6-a0a4-42407ccf224c', '').
narrative_ontology:cs_kernel_id(border_normative_status__sovereignty_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, citizen_polity).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, internal_displacement_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A political community whose members claim the right to collectively determine membership and resource allocation through democratic process. This reading grants them foundational authority to exclude non-members and to structure internal distribution according to their values. They articulate border enforcement and exclusion as legitimate expressions of self-determination, not as coercive extraction.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, citizen_polity, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(border_normative_status__sovereignty_primary, citizen_polity, agenda_setter).

% The formal state that administers and enforces the border constraint. Under this reading, the state is the legitimate executor of the polity's self-determination claim. Border enforcement is framed as a core sovereign function, not as discretionary coercion. The apparatus claims authority to exclude and to remove non-compliant residents.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, state_institutional_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Non-citizens who seek entry but are denied, or who enter without authorization and are subject to enforcement. This reading structures them as external to the self-determining polity, thus their exclusion is not treated as extraction from members but as the boundary maintenance prerequisite for self-determination to function. They bear the cost of enforcement (detention, deportation, forcible separation from kin already inside) without voice in the legitimating democratic process.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Citizens or long-term residents displaced by state development projects, resource extraction, or urban renewal pursued in the name of collective interest. Under this reading, such displacement is treated as an internal matter of collective priority-setting, not as a violation requiring consent or compensation. They are formally members but structurally lack veto power over arrangements that remove them.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, internal_displacement_populations, payer,
    powerless, biographical, constrained, national).

% Other states that recognize one another's border legitimacy and reciprocally enforce mutual exclusion. They validate the framework through their own practices, creating a nested legitimacy structure where each state's exclusion authority depends on recognizing all other states' exclusion authority.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, neighboring_polities, observer,
    institutional, generational, analytical, national).

% International organizations and NGOs that frame freedom of movement as a human right and challenge exclusion on grounds of disproportionality and non-consent. They are structurally excluded from the sovereignty-primary reading's legitimacy framework—their objections are reframed as external impositions on self-determination rather than valid competing claims. Their voice is present but dismissed as illegitimate interference.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, human_rights_advocacy_bodies, excluded,
    organized, generational, constrained, global).

% The tradition of political thought grounding legitimacy in popular sovereignty and collective self-determination (Rousseau, Social Contract theorists, modern nationalism). Not an agent, but a vindicated proposition that shapes how the constraint is justified and authorized.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, philosophical_lineage, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(border_normative_status__sovereignty_primary, philosophical_lineage).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__sovereignty_primary, state_institutional_apparatus).
narrative_ontology:fixing_cost_class(border_normative_status__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes and maintains bounded political communities by defining membership and enforcing the boundary; enables collective democratic self-governance by limiting the demos to a defined set of decision-makers; solves the foundational problem of 'who gets to decide' by rooting authority in a delimited national or territorial polity.
% TRANSFER_FUNCTION: Moves enforcement labor, coercive capacity, and police/military resources toward border maintenance and exclusion/deportation operations. Transfers claim-making and voice rights FROM excluded populations TO citizen-members who are compensated by having a secured, bounded territory where their preferences determine policy.
% ABSENT_VOICES: Excluded migrants and displaced populations are the primary absent voices—the constraint is structured so that those who bear its costs have no voice in the democratic process that legitimates it. Human rights bodies and transnational advocacy networks would argue for proportionality constraints and consent-based migration, but are categorically excluded from the sovereignty-primary framework as external impositions. Migrants in neighboring countries affected by displacement flows are also absent.
% DISAPPEARANCE_RATIONALE: If the sovereignty-primary border constraint vanished overnight—if polities lost the authority to exclude non-members—the political world would undergo structural reorganization: nation-states as currently constituted depend on bounded membership for their legitimacy and resource-allocation model; without border authority, the definition of 'the polity' and the mechanism for collective decision-making would be thrown into contest. Labor mobility, settlement patterns, and resource distribution would shift as friction to movement disappeared.
% FOUNDING_PROBLEM: How can multiple sovereign peoples share planetary space without subordinating all peoples to a single authority? How does a polity retain the capacity for self-governance and cultural reproduction against the infinite demands and flows of the global population?
% FOUNDING_PROBLEM_CORROBORATION: State actors and political theorists committed to the sovereignty framework attest the problem is live and foundational to stable political order. International law doctrine (Westphalian principle of exclusive territorial sovereignty) embeds this assumption as the baseline. Critics from human rights and transnational advocacy bodies contest that the problem IS live only because the sovereignty framework itself creates it—but even critics grant that states DO exercise border authority and that this authority is experienced as legitimate by most citizen-members. The founding problem is corroborated by state practice and by the observed fact that populations invest politically in defending their borders; the interpretation of what this fact means (legitimate self-determination vs. constructed scarcity) is the reading contest itself.
narrative_ontology:disappearance_verdict(border_normative_status__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__sovereignty_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__sovereignty_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_normative_status__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__sovereignty_primary, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness (0.68) is substantial because the constraint concentrates residence rights and political voice in the citizen-member set, systematically excluding non-members from both goods. Suppression (0.71) is high because the constraint depends on active enforcement to maintain the boundary—border guards, detention facilities, deportation machinery—and on legal barriers that make entry and residence impossible for excluded populations absent state permission. Theater ratio (0.42 and rising from 0.18 in 1945) reflects the increasing proportion of border maintenance activity that is performative or symbolic rather than functionally necessary. Early postwar borders (1945) primarily controlled goods and military threat; by 2025, border enforcement includes immigration theater (performative enforcement increases or decreases to signal toughness) and symbolic sovereignty performance. Accessibility collapse (0.73) is high because once the framework is understood, alternatives to border-enforced membership close off: excluded populations cannot credibly claim insider status, legal pathways are narrow, and informal settlement faces ongoing threat of removal. Resistance (0.64) is substantial because the constraint meets active resistance from migrant networks, human rights organizations, and some segments of citizen polities that question the framework. The measurement series run from 1945 (Westphalian consolidation post-WWII) to 2025 (contemporary era), showing extractiveness and suppression both rising monotonically over 80 years, indicating that the constraint has intensified rather than moderated.
 *
 * PERSPECTIVAL GAP:
 *   The citizen-polity and state-apparatus seats and the excluded-migrant seats should compute to different types from the engine's structural analysis. From the citizen seat, the arrangement is genuine coordination—members genuinely benefit from collective control over membership and can frame it as self-determination, not extraction. From the excluded-migrant seat, the same structure operates as enforced extraction—they bear costs of enforcement and access denial without voice or consent. From neighboring-polity and human-rights-body seats, the constraint appears as either legitimate mutual recognition (neighboring-state view) or illegitimate dominance (rights-advocacy view). The authored claimed_type is tangled_rope, which presumes both coordination (for members) and asymmetric extraction (from excluded populations). The engine computes per-seat types from directionality (d) values derived from power and exit options: member polities sit near d=0.0 (beneficiary), excluded migrants near d=1.0 (full target), and human-rights bodies at d varies depending on whether their institutionalization gives them effective power.
 *
 * DIRECTIONALITY LOGIC:
 *   The citizen-polity beneficiary group (organized power, biographical+ time horizon, mobile exit within the system) derives d near 0.0 because they collect benefits (residence rights, political voice, resource-allocation priority) from the constraint. Excluded migrants (powerless, trapped exit, biographical horizon) derive d near 1.0 because they bear extraction (denial of residence, enforcement costs, forcible separation) and have no structural exit short of international relocation or accepting subzero status. Internal displacement populations are also targets but with constrained rather than trapped exit (they are formally members but lack veto power)—their d sits near 0.8–0.85. State apparatus is agenda-setter (institutional power, analytical exit) with d modulated by whether it internalizes member preferences or extracts independently; the reading treats it as transparent executor so d stays near the beneficiary end through the beneficiary group it serves.
 *
 * MANDATROPHY ANALYSIS:
 *   The sovereignty-primary reading avoids the false-summit problem (a mountain claiming natural-law status while identifiable beneficiaries exist) because it is claimed as tangled_rope, not mountain. It is a constructed doctrine that benefits the citizen polity and the state apparatus; the beneficiary declaration is explicit. The reading DOES face a mandatrophy question: has the founding problem it solves (how to enable collective self-governance across multiple polities) persisted in the same form for 80 years, or has the problem evolved (globalization, migration pressure, climate displacement, refugee flows) while the constraint's justification has remained rhetorically static? The measurement series showing rising theater_ratio (0.18 to 0.42) and rising suppression_requirement (0.48 to 0.71) over 80 years suggest that enforcement intensity has increased faster than coordination value, which is a mandatrophy candidate. The founding_problem_status is declared 'live' because state actors continue to invoke the problem as justification, but the corroboration includes critics who say the problem is an artifact of the sovereignty framework itself, not a pre-existing constraint on governance. This reading does not trigger mandatrophy resolution (the constraint's coordination function—enabling collective determination—persists as valid), but it is a candidate for monitoring via the T17 measurement trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is the sovereignty-primary reading describing a natural political boundary or a constructed doctrine that benefits particular actors?',
    'Cross-reading comparative structural analysis: compare this reading''s ε, extraction targets, and beneficiary structure to the freedom_primary and qualified_sovereignty readings. A stable ε-invariance (same referent assessed differently by each reading) indicates the contest is about interpretation; an ε shift indicates different constraints.',
    'If ε is reading-indexed (different readings genuinely assess the same constraint differently per OQ-26), the three readings form a kernel family with independent type classifications per seat. If ε shifts, the readings describe different constraints (ε-invariance principle, DP-001) and should be decomposed as separate stories.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Whether sovereignty-primary is a reading of a contested kernel or a distinct constraint with its own ε.').

omega_variable(
    legitimacy_grounding_empirical,
    'Does the sovereignty-primary reading rest on an empirical claim about what polities require to function (the founding problem is real), or on a deontological commitment (polities SHOULD be sovereign, regardless of empirical necessity)?',
    'Analysis of how advocates respond to scenarios where self-determination and coordination might be maintained without exclusion (e.g., federated global governance with sub-demos; open borders with redistribution). If advocates shift to deontological framing when empirical arguments fail, the axiom is deontological; if they maintain that coordination IS impossible without closure, it is empirically_contingent.',
    'If the axiom is empirically_contingent, evidence that open systems can coordinate threatens it (axiom_overriding drift). If deontological, empirical evidence does not foreclose it; it remains holdable even if coordination succeeds without closure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_grounding_empirical, conceptual, 'Whether the legitimacy claim rests on empirical necessity or deontological principle.').

omega_variable(
    internal_displacement_victim_status,
    'Are internally displaced populations genuine victims of the sovereignty-primary constraint, or are they affected by a different constraint (internal resource allocation) that is orthogonal to the border constraint?',
    'Structural analysis: if displacement occurs BECAUSE the polity invokes collective self-determination to override individual relocation consent, then the border constraint''s extraction logic (collective choice overrides individual choice) directly causes the displacement—they are victims of the SAME constraint. If displacement occurs through a distinct mechanism (eminent domain, resource conflict) that would operate regardless of border status, they are victims of a different constraint.',
    'If displacement is a direct extraction from the sovereignty-primary constraint, it strengthens the Tangled Rope classification (asymmetric coordination—members benefit from self-determination, displaced populations pay). If displacement is orthogonal, internal_displacement_populations should be removed from the victims array and the constraint reclassified as targeting excluded_migrants only.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_displacement_victim_status, empirical, 'Whether internal displacement is structurally caused by the border constraint or by orthogonal mechanisms.').

omega_variable(
    exclusion_mechanism_internalization,
    'Is the measured suppression (0.71) structural (border enforcement infrastructure, legal barriers to entry/residence) or internalized (excluded populations have internalized their own illegitimacy, lack information about alternatives, or experience psychological barriers independent of external barriers)?',
    'Post-border analysis: if barriers to exit/entry are removed but excluded populations still remain excluded by psychological identification, belief in their own unworthiness, or lack of knowledge about alternatives, suppression is internalized. If removal of barriers produces rapid movement and settlement, suppression is primarily structural.',
    'If suppression is internalized, the constraint''s effective extraction is higher than the structural measure suggests—the target carries the suppression beyond the enforcement boundary. If structural, remedies focused on barrier removal would reduce extractiveness; if internalized, remedies must address belief and identification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exclusion_mechanism_internalization, empirical, 'Structural vs. internalized suppression mechanism in the sovereignty-primary border constraint.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the sovereignty-primary reading logically foreclose the freedom_primary reading within any single coherent framework, or do they coexist as competing normative commitments held by different political factions?',
    'Logical analysis: if the core premise of sovereignty-primary (states have foundational authority to exclude) directly contradicts the core premise of freedom_primary (freedom of movement is inalienable), and no single framework can hold both, foreclosure applies. If frameworks exist where both coexist (e.g., qualified sovereignty: states may exclude but only within proportionality constraints, which preserves some freedom), they coexist rather than foreclose.',
    'Foreclosure is rare and signals fundamental conceptual incompatibility. Coexistence is typical and signals that the readings are live positions held by competing political factions. The relation type affects how the engine models the kernel contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical relationship between sovereignty-primary and freedom-primary readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__sovereignty_primary, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1945, border_normative_status__sovereignty_primary, theater_ratio, 1945, 0.18).
narrative_ontology:measurement(bord_tr_t1975, border_normative_status__sovereignty_primary, theater_ratio, 1975, 0.22).
narrative_ontology:measurement(bord_tr_t1995, border_normative_status__sovereignty_primary, theater_ratio, 1995, 0.28).
narrative_ontology:measurement(bord_tr_t2010, border_normative_status__sovereignty_primary, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(bord_tr_t2020, border_normative_status__sovereignty_primary, theater_ratio, 2020, 0.4).
narrative_ontology:measurement(bord_tr_t2025, border_normative_status__sovereignty_primary, theater_ratio, 2025, 0.42).

% Extraction over time
narrative_ontology:measurement(bord_be_t1945, border_normative_status__sovereignty_primary, base_extractiveness, 1945, 0.52).
narrative_ontology:measurement(bord_be_t1975, border_normative_status__sovereignty_primary, base_extractiveness, 1975, 0.58).
narrative_ontology:measurement(bord_be_t1995, border_normative_status__sovereignty_primary, base_extractiveness, 1995, 0.63).
narrative_ontology:measurement(bord_be_t2010, border_normative_status__sovereignty_primary, base_extractiveness, 2010, 0.66).
narrative_ontology:measurement(bord_be_t2020, border_normative_status__sovereignty_primary, base_extractiveness, 2020, 0.67).
narrative_ontology:measurement(bord_be_t2025, border_normative_status__sovereignty_primary, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1945, border_normative_status__sovereignty_primary, suppression_requirement, 1945, 0.48).
narrative_ontology:measurement(bord_su_t1975, border_normative_status__sovereignty_primary, suppression_requirement, 1975, 0.54).
narrative_ontology:measurement(bord_su_t1995, border_normative_status__sovereignty_primary, suppression_requirement, 1995, 0.61).
narrative_ontology:measurement(bord_su_t2010, border_normative_status__sovereignty_primary, suppression_requirement, 2010, 0.67).
narrative_ontology:measurement(bord_su_t2020, border_normative_status__sovereignty_primary, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(bord_su_t2025, border_normative_status__sovereignty_primary, suppression_requirement, 2025, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__sovereignty_primary, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(border_normative_status__sovereignty_primary, 0.14).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, border_normative_status__freedom_primary).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, border_normative_status__qualified_sovereignty).

% DUAL FORMULATION NOTE:
% This constraint is one reading of a contested kernel (border_normative_status). The kernel has three readings, each instantiating a different constraint with different beneficiary/victim structures and different type classifications per seat. The three readings form a constraint family linked via network.affects_constraints. Sibling readings: freedom_primary (freedom of movement as foundational right), qualified_sovereignty (proportionality-constrained border authority). The ε-invariance principle applies: each reading assesses the SAME referent (the standing arrangement of border enforcement and membership exclusion) from its own reading's perspective, and ε is reading-indexed (per OQ-26). The structural distinction is encoded in the reading_relations and axioms blocks in cs_structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
