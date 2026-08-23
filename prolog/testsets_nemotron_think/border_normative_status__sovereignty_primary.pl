% ============================================================================
% CONSTRAINT STORY: border_normative_status__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
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
 *   constraint_id: border_normative_status__sovereignty_primary
 *   human_readable: Sovereignty-Primary Border Authority (Self-Determination Reading)
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the sovereignty-primary reading of the
 *   contested kernel 'border_normative_status.' The reading asserts that
 *   territorial boundaries are legitimate instruments of collective
 *   self-determination and that states possess foundational authority to
 *   exclude non-members. It claims the constraint type 'rope' (pure
 *   coordination for self-determination) while the authored metrics reveal
 *   substantial extraction from excluded migrants and active enforcement — a
 *   structural profile the engine will compute as tangled_rope. The
 *   claim/metric divergence is deliberate: the reading's self-understanding
 *   vs. the structural reality it produces. The kernel has two other
 *   readings: freedom_primary (borders as rights violations) and
 *   qualified_sovereignty (borders as limited, proportional authority). This
 *   story authors only the sovereignty-primary reading per the ε-invariance
 *   principle.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__sovereignty_primary, 0.62).
domain_priors:suppression_score(border_normative_status__sovereignty_primary, 0.71).
domain_priors:theater_ratio(border_normative_status__sovereignty_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, extractiveness, 0.62).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, accessibility_collapse, 0.63).
narrative_ontology:constraint_metric(border_normative_status__sovereignty_primary, resistance, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__sovereignty_primary, rope).
narrative_ontology:human_readable(border_normative_status__sovereignty_primary, "Sovereignty-Primary Border Authority (Self-Determination Reading)").
narrative_ontology:topic_domain(border_normative_status__sovereignty_primary, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_normative_status__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__sovereignty_primary, 'adba6d1e-5020-43d6-9c3c-4e2d01fcae95').
narrative_ontology:cs_kernel_codification('adba6d1e-5020-43d6-9c3c-4e2d01fcae95', distributed).
narrative_ontology:cs_authority_grounding('adba6d1e-5020-43d6-9c3c-4e2d01fcae95', lineage).
narrative_ontology:cs_interpretation_layer_present('adba6d1e-5020-43d6-9c3c-4e2d01fcae95').
narrative_ontology:cs_reading_relation('adba6d1e-5020-43d6-9c3c-4e2d01fcae95', border_normative_status__freedom_primary, coexists_with).
narrative_ontology:cs_reading_relation('adba6d1e-5020-43d6-9c3c-4e2d01fcae95', border_normative_status__qualified_sovereignty, influences).
narrative_ontology:cs_axiom('adba6d1e-5020-43d6-9c3c-4e2d01fcae95', foundational, state_exclusion_authority_foundational).
narrative_ontology:cs_axiom_status(state_exclusion_authority_foundational, holdable).
narrative_ontology:cs_axiom_grounding('adba6d1e-5020-43d6-9c3c-4e2d01fcae95', state_exclusion_authority_foundational, deontological).
narrative_ontology:cs_axiom('adba6d1e-5020-43d6-9c3c-4e2d01fcae95', secondary, collective_self_determination_requires_border_control).
narrative_ontology:cs_axiom_status(collective_self_determination_requires_border_control, holdable).
narrative_ontology:cs_axiom_grounding('adba6d1e-5020-43d6-9c3c-4e2d01fcae95', collective_self_determination_requires_border_control, deontological).
narrative_ontology:cs_reference_frame('adba6d1e-5020-43d6-9c3c-4e2d01fcae95', westphalian_sovereignty).
narrative_ontology:cs_drift_state('adba6d1e-5020-43d6-9c3c-4e2d01fcae95', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('adba6d1e-5020-43d6-9c3c-4e2d01fcae95', '').
narrative_ontology:cs_kernel_id(border_normative_status__sovereignty_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, states).
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, citizen_polities).
narrative_ontology:constraint_beneficiary(border_normative_status__sovereignty_primary, territorial_communities).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, asylum_seekers).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, stateless_persons).
narrative_ontology:constraint_victim(border_normative_status__sovereignty_primary, displaced_non_citizens).
narrative_ontology:constraint_vindicates(border_normative_status__sovereignty_primary, state_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(border_normative_status__sovereignty_primary, collective_self_determination_principle).
narrative_ontology:constraint_vindicates(border_normative_status__sovereignty_primary, territorial_integrity_norm).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Claim foundational authority to control borders as expression of popular sovereignty. Set admission criteria, enforce exclusion, and define the political community. Benefit from the legitimacy and control this authority provides. Can modify border policy through domestic law and international agreements; exit from the constraint would mean relinquishing sovereign control.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, states, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(border_normative_status__sovereignty_primary, states, beneficiary).

% The constituted political community that exercises self-determination through border control. Gain collective autonomy, cultural continuity, and democratic self-governance from the ability to define membership. Bear costs of enforcement through taxation. Exit is constrained by nationality and territorial attachment; leaving the polity requires migration to another state.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, citizen_polities, beneficiary,
    organized, biographical, constrained, national).

% Sub-state communities (regions, municipalities, indigenous groups) that benefit from the state's border authority as it protects their local way of life, resources, and political distinctiveness. Their situation varies: some gain protection, others experience border enforcement as disruptive to cross-border ties. Exit options are constrained by local rootedness.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, territorial_communities, beneficiary,
    moderate, biographical, constrained, local).

% Individuals denied entry or legal status by border enforcement. Bear the full cost of exclusion: denied life opportunities, family separation, exposure to danger in countries of origin, precarious existence in transit. Have no meaningful exit from the constraint — they cannot appeal to a higher authority that overrides state exclusion, and return to origin may be impossible or fatal. Their voice is structurally absent from the sovereign decision.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% A subset of excluded migrants with well-founded fear of persecution. International law nominally protects their right to seek asylum, but the sovereignty-primary reading treats this as a discretionary exception rather than a constraint on exclusion authority. They bear extreme costs: detention, refoulement, death. Exit options are near-zero; the constraint actively prevents their movement to safety.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, asylum_seekers, payer,
    powerless, immediate, trapped, global).

% Individuals with no recognized nationality, excluded by all states. The sovereignty-primary reading treats statelessness as a gap between sovereign authorities rather than a failure of the system — each state's exclusive authority to define its membership leaves no positive obligation to admit the stateless. They bear permanent exclusion with no polity to claim them. Exit is structurally impossible.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, stateless_persons, payer,
    powerless, generational, trapped, global).

% Long-term residents, guest workers, and their families who lack citizenship but have built lives within a territory. The sovereignty-primary reading treats their displacement (deportation, denial of status) as a legitimate exercise of exclusion authority — their ties are an externality. They bear severe costs but have slightly more exit options than the stateless (voluntary return, regularization campaigns). Their voices are excluded from the sovereign decision.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, displaced_non_citizens, payer,
    powerless, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(border_normative_status__sovereignty_primary, displaced_non_citizens, excluded).

% Treaty bodies, UN special procedures, regional courts that monitor state compliance with human rights obligations. They observe the constraint's operation, document its impact on excluded migrants, and issue findings — but lack enforcement power against sovereign primary claims. Their analytical seat sees the full structure: coordination for citizens, extraction from the excluded.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% Civil society organizations, legal aid networks, transnational advocacy groups that represent excluded migrants. They would object to the sovereignty-primary reading's legitimization of exclusion but are structurally excluded from the sovereign decision-making arena. Their influence operates through shame, litigation, and public pressure — not through the constraint's internal logic.
narrative_ontology:constraint_stakeholder(border_normative_status__sovereignty_primary, migrant_advocacy_networks, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of defining the political community that can exercise collective self-determination: who constitutes the 'self' that determines. Border authority provides a clear, enforceable boundary between members and non-members, enabling democratic governance, resource allocation, and cultural continuity within a defined territory.
% TRANSFER_FUNCTION: Moves the power to decide membership and the benefits of political inclusion (rights, protections, resources, voice) from a universal human pool to a bounded citizenry. The cost — exclusion, denial of entry, precarity — is transferred to non-members, particularly those seeking admission. The transfer is enforced by state coercion at borders and through internal immigration control.
% ABSENT_VOICES: Excluded migrants, asylum seekers, stateless persons, and displaced non-citizens are structurally absent from the sovereign decision that defines them as outsiders. They have no vote, no standing in the constituent power, no access to the deliberative process that draws the boundary. International human rights bodies and migrant advocacy networks speak for them but are excluded from the authoritative interpretation of the constraint.
% DISAPPEARANCE_RATIONALE: If the sovereignty-primary border norm vanished overnight, the global migration regime would fundamentally reorganize: states would lose their foundational claim to exclude, opening pathways for universal freedom of movement claims; the citizen/non-citizen distinction would lose its normative anchor; international law would shift from state-centric to person-centric; the political architecture of the last 350 years would require reconstruction.
% FOUNDING_PROBLEM: The Westphalian order's founding problem: how to end religious war and establish political order in a fragmented Europe. The solution was territorial sovereignty — each prince determines the religion (later, the political constitution) of his territory, with borders as the line where his authority becomes exclusive. Collective self-determination later inherited this structure: the 'people' replaces the prince, but the border remains the instrument of their self-rule.
% FOUNDING_PROBLEM_CORROBORATION: The sovereignty-primary reading's genealogy is attested by state practice, diplomatic history, and international legal doctrine (all within the beneficiary set). Critics from outside — postcolonial scholars, migration theorists, human rights lawyers — argue the founding problem was never 'how to enable self-determination' but 'how to entrench European state power,' and that the colonial history of borders (drawn without consent of bordered peoples) disproves the self-determination narrative. No corroboration from outside the beneficiary set for the claim that borders *as currently drawn and enforced* serve collective self-determination.
narrative_ontology:disappearance_verdict(border_normative_status__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__sovereignty_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(border_normative_status__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__sovereignty_primary, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness (0.62) reflects that the border regime transfers life opportunities from excluded migrants to citizen polities — the standing arrangement under contest is assessed by this reading as having moderate extraction because the reading sees exclusion as legitimate exercise of authority, not extraction. But the structural data (victims array) shows the extraction is real. Suppression (0.71) is high because the constraint depends on active border enforcement, detention, deportation — without coercion, the boundary collapses. Theater ratio (0.28) is low-moderate: border enforcement has real coordination function (defining the polity) but growing performative elements (symbolic walls, deterrence theater). Accessibility collapse (0.63) reflects that the sovereignty-primary norm makes open borders conceptually incoherent within its framework. Resistance (0.54) is moderate: the reading faces sustained contestation from freedom_primary and qualified_sovereignty readings, but the institutional architecture of sovereignty remains dominant.
 *
 * PERSPECTIVAL GAP:
 *   From the state/citizen seat, the constraint appears as rope: a genuine coordination mechanism that solves the 'who constitutes the people' problem. From the excluded migrant seat, it appears as snare: pure extraction enforced by violence, with the coordination story as cover. From the international body seat, it appears as tangled_rope: real coordination function (order, stability) married to asymmetric extraction (rights denial). The engine computes this divergence from the structural data; the authored claim (rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   States and citizen_polities are structural beneficiaries (d near 0.0) — they collect the gains of self-determination, control the rule-setting, and have arbitrage-grade exit (can modify the constraint through law/diplomacy). Excluded_migrants, asylum_seekers, stateless_persons are full targets (d near 1.0) — trapped, powerless, bearing the full cost of exclusion with no voice. Territorial_communities sit near symmetric (d ~0.5) — they benefit from protection but bear local costs. Displaced_non_citizens are constrained targets (d ~0.75) — some exit via regularization but structurally excluded. International_human_rights_bodies and migrant_advocacy_networks are analytical/excluded observers (d ~0.25/0.5) — they see the structure but cannot change it from within.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Westphalian order, ending religious war) is historically dead — the conditions that made territorial sovereignty the solution no longer obtain. But the arrangement persists and has been repurposed for democratic self-determination. The status is 'contested' because beneficiaries (states) claim the problem is live (self-determination requires borders) while excluded parties and external critics argue the mandate has atrophied into a mechanism for hoarding privilege. The classification prevents mislabeling by surfacing the victim set: if this were pure coordination, there would be no structurally excluded class bearing costs without voice. The presence of victims with trapped exit makes it at minimum tangled_rope, not rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is the sovereignty_primary reading a distinct constraint from the kernel ''border_normative_status,'' or does it represent the kernel itself?',
    'Structural decomposition test: if freedom_primary and qualified_sovereignty produce different ε values, different victim sets, and different enforcement logics when applied to the same referent (the standing border regime), then the kernel is a label covering multiple constraints. The ε-invariance principle requires separate stories.',
    'If the kernel is one constraint with observer-dependent classification, the engine''s per-seat computation would handle the divergence. If it decomposes into three constraints (as authored here), each gets its own ε, stakeholders, and type — and they are linked via network.affects_constraints. This story assumes decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel label covers one constraint or a constraint family.').

omega_variable(
    sovereignty_primary_vs_freedom_primary_foreclosure,
    'Does the sovereignty_primary reading logically foreclose the freedom_primary reading within a single commitment framework, or do they merely coexist as competing political positions?',
    'Test: can a single legal-political framework simultaneously hold that (a) states have foundational authority to exclude non-members as an exercise of collective self-determination, AND (b) freedom of movement is a fundamental human right that borders impermissibly restrict? If no framework can hold both as foundational, the relation is ''forecloses.'' If different parties hold each while the framework remains unsettled, ''coexists_with.''',
    'If ''forecloses,'' the engine treats the readings as mutually exclusive attractors — adopting one collapses the other. If ''coexists_with,'' both remain live in the corpus with contamination edges. This story authors ''coexists_with'' based on the empirical fact that international law contains both sovereignty and human rights commitments in unresolved tension.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_primary_vs_freedom_primary_foreclosure, conceptual, 'Logical relationship between sovereignty_primary and freedom_primary readings.').

omega_variable(
    extraction_legitimacy_gap,
    'From the sovereignty_primary reading''s own lights, is the extraction borne by excluded migrants a legitimate cost of coordination (like taxation) or an illegitimate byproduct?',
    'Internal doctrinal analysis: does the reading''s normative framework contain a principle that limits extraction (e.g., proportionality, necessity, non-refoulement) or does it treat exclusion as categorically authorized? The qualified_sovereignty reading answers ''limited''; sovereignty_primary''s answer determines whether its claimed_type ''rope'' is internally coherent.',
    'If the reading internally limits extraction, its claimed_type ''rope'' may be sincere. If it treats exclusion as categorically authorized, the victim set is a structural feature, not a bug — making the claimed_type ''rope'' a false summit. This omega routes the question to the engine''s false_summit_mountain detection (adapted for rope claims).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_legitimacy_gap, conceptual, 'Whether the reading''s own normative logic admits limits on extraction from the excluded.').

omega_variable(
    colonial_border_legitimacy,
    'Does the sovereignty_primary reading''s claim that borders serve collective self-determination apply to borders drawn by colonial powers without consent of the bordered peoples?',
    'Historical-material analysis: if the reading''s legitimacy claim depends on the border expressing the will of a self-determining people, then colonial borders (most of the Global South) lack that legitimacy. The reading must either (a) restrict its claim to ''legitimate borders only'' (shrinking its scope), (b) argue colonial borders were ratified by post-independence peoples, or (c) accept that its claim covers illegitimate borders too (exposing extraction).',
    'If the reading cannot account for colonial borders without admitting extraction, its claimed_type ''rope'' is undermined globally. The victim set expands to include entire populations subjected to borders they never consented to. This is a major scope ambiguity for the constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(colonial_border_legitimacy, empirical, 'Whether the sovereignty-primary legitimacy claim survives contact with the colonial history of most existing borders.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__sovereignty_primary, 1648, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(border_sov_primary_tr_t1648, border_normative_status__sovereignty_primary, theater_ratio, 1648, 0.1).
narrative_ontology:measurement(border_sov_primary_tr_t1789, border_normative_status__sovereignty_primary, theater_ratio, 1789, 0.12).
narrative_ontology:measurement(border_sov_primary_tr_t1885, border_normative_status__sovereignty_primary, theater_ratio, 1885, 0.18).
narrative_ontology:measurement(border_sov_primary_tr_t1945, border_normative_status__sovereignty_primary, theater_ratio, 1945, 0.22).
narrative_ontology:measurement(border_sov_primary_tr_t1975, border_normative_status__sovereignty_primary, theater_ratio, 1975, 0.25).
narrative_ontology:measurement(border_sov_primary_tr_t2000, border_normative_status__sovereignty_primary, theater_ratio, 2000, 0.27).
narrative_ontology:measurement(border_sov_primary_tr_t2025, border_normative_status__sovereignty_primary, theater_ratio, 2025, 0.28).

% Extraction over time
narrative_ontology:measurement(border_sov_primary_be_t1648, border_normative_status__sovereignty_primary, base_extractiveness, 1648, 0.35).
narrative_ontology:measurement(border_sov_primary_be_t1789, border_normative_status__sovereignty_primary, base_extractiveness, 1789, 0.4).
narrative_ontology:measurement(border_sov_primary_be_t1885, border_normative_status__sovereignty_primary, base_extractiveness, 1885, 0.55).
narrative_ontology:measurement(border_sov_primary_be_t1945, border_normative_status__sovereignty_primary, base_extractiveness, 1945, 0.58).
narrative_ontology:measurement(border_sov_primary_be_t1975, border_normative_status__sovereignty_primary, base_extractiveness, 1975, 0.6).
narrative_ontology:measurement(border_sov_primary_be_t2000, border_normative_status__sovereignty_primary, base_extractiveness, 2000, 0.61).
narrative_ontology:measurement(border_sov_primary_be_t2025, border_normative_status__sovereignty_primary, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(border_sov_primary_su_t1648, border_normative_status__sovereignty_primary, suppression_requirement, 1648, 0.4).
narrative_ontology:measurement(border_sov_primary_su_t1789, border_normative_status__sovereignty_primary, suppression_requirement, 1789, 0.5).
narrative_ontology:measurement(border_sov_primary_su_t1885, border_normative_status__sovereignty_primary, suppression_requirement, 1885, 0.65).
narrative_ontology:measurement(border_sov_primary_su_t1945, border_normative_status__sovereignty_primary, suppression_requirement, 1945, 0.68).
narrative_ontology:measurement(border_sov_primary_su_t1975, border_normative_status__sovereignty_primary, suppression_requirement, 1975, 0.7).
narrative_ontology:measurement(border_sov_primary_su_t2000, border_normative_status__sovereignty_primary, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(border_sov_primary_su_t2025, border_normative_status__sovereignty_primary, suppression_requirement, 2025, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__sovereignty_primary, identity_coordination).
narrative_ontology:boltzmann_floor_override(border_normative_status__sovereignty_primary, 0.08).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, border_normative_status__freedom_primary).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, border_normative_status__qualified_sovereignty).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, refugee_protection_regime).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, citizenship_acquisition_rules).
narrative_ontology:affects_constraint(border_normative_status__sovereignty_primary, internal_migration_controls).

% DUAL FORMULATION NOTE:
% This constraint is one member of the border_normative_status kernel family. The three readings (sovereignty_primary, freedom_primary, qualified_sovereignty) decompose the colloquial 'border norm' into structurally distinct constraints with different ε, different victim sets, and different enforcement logics. They are linked via affects_constraints. The sovereignty_primary reading claims the coordination function (self-determination) and treats exclusion as its legitimate instrument. The freedom_primary reading claims the extraction function (rights violation) and treats coordination as cover. The qualified_sovereignty reading occupies the intermediate zone where both functions are acknowledged and balanced. The ε values differ substantially: sovereignty_primary ε ≈ 0.62 (this story), freedom_primary ε ≈ 0.85 (extraction-dominant), qualified_sovereignty ε ≈ 0.45 (balanced).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_normative_status__sovereignty_primary, institutional, 0.05).
constraint_indexing:directionality_override(border_normative_status__sovereignty_primary, organized, 0.15).
constraint_indexing:directionality_override(border_normative_status__sovereignty_primary, moderate, 0.35).
constraint_indexing:directionality_override(border_normative_status__sovereignty_primary, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
