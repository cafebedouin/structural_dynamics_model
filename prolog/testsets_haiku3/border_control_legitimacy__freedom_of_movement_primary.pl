% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__freedom_of_movement_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_control_legitimacy__freedom_of_movement_primary, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: border_control_legitimacy__freedom_of_movement_primary
 *   human_readable: Border Control Authority Under Freedom of Movement (Freedom-Primary Reading)
 *   domain: political/legal/migration
 *
 * SUMMARY:
 *   This constraint story instantiates the freedom-of-movement primary
 *   reading of the contested kernel 'border_control_legitimacy'. Under this
 *   reading, freedom of movement is asserted as a fundamental human right
 *   binding on all states, and territorial sovereignty is reinterpreted as
 *   authority to regulate rights and obligations within territory, NOT
 *   authority to exclude on the basis of movement. The state apparatus that
 *   enforces border closure is thus delegitimized in its exclusion function
 *   (though its regulatory function remains valid). Victims are those denied
 *   movement: excluded migrants, displaced workers, separated families,
 *   asylum seekers. Beneficiaries are incumbent citizens and powerful
 *   receiving jurisdictions that capture rents from exclusion. The constraint
 *   is classified as Tangled Rope: genuine coordination function (allocating
 *   membership and welfare distribution) plus asymmetric extraction (negating
 *   movement rights to concentrate opportunity). The claim/metric gap is
 *   intentional: freedom-primary reading CLAIMS the constraint should be Rope
 *   or even delegitimized entirely; the authored metrics describe it as
 *   substantially extractive and suppressive. The engine's per-seat
 *   computation will show how beneficiary and victim seats experience this
 *   constraint differently.
 *
 * KEY AGENTS:
 *   - incumbent_citizens (organized, beneficiary): retain labor-market protection and welfare priority
 *   - state_administrative_apparatus (institutional, agenda-setter): administers exclusion and enforcement; legitimacy questioned under this reading
 *   - excluded_migrants (powerless, payer): denied entry and movement; trapped exit
 *   - displaced_workers (powerless, payer, identity-locked): excluded from higher-income labor; identity fused to constrained material existence
 *   - separated_families (powerless, payer, identity-locked): relational identity fractured by enforcement
 *   - asylum_seekers (powerless, payer/excluded, trapped): fled persecution but turned away at borders
 *   - supranational_human_rights_bodies (institutional, observer): theoretically uphold freedom of movement but lack enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__freedom_of_movement_primary, 0.82).
domain_priors:suppression_score(border_control_legitimacy__freedom_of_movement_primary, 0.87).
domain_priors:theater_ratio(border_control_legitimacy__freedom_of_movement_primary, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, extractiveness, 0.82).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(border_control_legitimacy__freedom_of_movement_primary, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__freedom_of_movement_primary, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__freedom_of_movement_primary, "Border Control Authority Under Freedom of Movement (Freedom-Primary Reading)").
narrative_ontology:topic_domain(border_control_legitimacy__freedom_of_movement_primary, "political/legal/migration").

domain_priors:requires_active_enforcement(border_control_legitimacy__freedom_of_movement_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__freedom_of_movement_primary, 'cfd22bc0-f48b-4b3d-b7e7-c868fffdd069').
narrative_ontology:cs_kernel_codification('cfd22bc0-f48b-4b3d-b7e7-c868fffdd069', fixed_text).
narrative_ontology:cs_authority_grounding('cfd22bc0-f48b-4b3d-b7e7-c868fffdd069', extraction).
narrative_ontology:cs_interpretation_layer_present('cfd22bc0-f48b-4b3d-b7e7-c868fffdd069').
narrative_ontology:cs_reading_relation('cfd22bc0-f48b-4b3d-b7e7-c868fffdd069', border_control_legitimacy__jurisdictional_sovereignty, influences).
narrative_ontology:cs_reading_relation('cfd22bc0-f48b-4b3d-b7e7-c868fffdd069', border_control_legitimacy__sovereignty_primary, forecloses).
narrative_ontology:cs_axiom('cfd22bc0-f48b-4b3d-b7e7-c868fffdd069', foundational, freedom_of_movement_inalienable).
narrative_ontology:cs_axiom_status(freedom_of_movement_inalienable, holdable).
narrative_ontology:cs_axiom_grounding('cfd22bc0-f48b-4b3d-b7e7-c868fffdd069', freedom_of_movement_inalienable, deontological).
narrative_ontology:cs_axiom('cfd22bc0-f48b-4b3d-b7e7-c868fffdd069', foundational, sovereignty_regulatory_not_exclusionary).
narrative_ontology:cs_axiom_status(sovereignty_regulatory_not_exclusionary, holdable).
narrative_ontology:cs_axiom_grounding('cfd22bc0-f48b-4b3d-b7e7-c868fffdd069', sovereignty_regulatory_not_exclusionary, deontological).
narrative_ontology:cs_reference_frame('cfd22bc0-f48b-4b3d-b7e7-c868fffdd069', universal_movement_right).
narrative_ontology:cs_drift_state('cfd22bc0-f48b-4b3d-b7e7-c868fffdd069', contemporary_enforcement_hardening, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('cfd22bc0-f48b-4b3d-b7e7-c868fffdd069', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, incumbent_citizens).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, state_administrative_apparatus).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, excluded_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, displaced_workers).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, separated_families).
narrative_ontology:constraint_victim(border_control_legitimacy__freedom_of_movement_primary, asylum_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__freedom_of_movement_primary, receiving_jurisdictions_labor_surplus).
narrative_ontology:constraint_vindicates(border_control_legitimacy__freedom_of_movement_primary, freedom_of_movement_as_human_right).
narrative_ontology:constraint_vindicates(border_control_legitimacy__freedom_of_movement_primary, limits_on_state_sovereignty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain labor-market protection, welfare distribution priority, and cultural continuity within the bordered territory. They experience border control as a coordination mechanism protecting their access to collective goods and institutional stability. Their preference for closure is articulated politically but enforced through state apparatus; exit is theoretically possible (emigration) but carries identity-dissolution costs.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, incumbent_citizens, beneficiary,
    organized, generational, mobile, national).

% Designs, administers, and enforces the border exclusion system. Justifies it as protecting institutional coherence, welfare integrity, and public order. Deploys inspection, documentation, and expulsion machinery. The apparatus's own legitimacy is increasingly questioned as enforcement intensifies; under the freedom-primary reading, its core function (exclusion by movement right negation) is delegitimized while its secondary function (regulating rights of those present) remains valid.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, state_administrative_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Are denied physical entry and movement despite possessing, under the freedom-primary reading, a basic human right to move. They bear the extraction directly: expulsion, detention, or prevention of arrival. No legal recourse within the excluding state; appeals go to supranational bodies (UN, regional courts) with limited enforcement power. Their trapped exit reflects both institutional barriers and the denial of the very movement freedom the reading asserts.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, excluded_migrants, payer,
    powerless, biographical, trapped, universal).

% Are excluded from labor markets in higher-income jurisdictions, preventing earnings that would materially alter their life prospects. The exclusion is justified by incumbent-citizen preferences and state labor-protection policy, not by their own incapacity or danger. Their identity fusion occurs through economic desperation: to abandon the attempt to cross is to accept a permanently constrained material existence as 'natural' or inevitable, embedding the exclusion into self-concept.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, displaced_workers, payer,
    powerless, biographical, identity_locked, global).

% Are kept apart by border enforcement when one member has citizenship and another does not. The relational identity (family continuity, caregiving bonds) is fractured by the enforcement machinery. Under the freedom-primary reading, this fracture violates not only the right to movement but the derivative right to family unity. Identity lock here manifests as the internalized belief that separation is a necessary cost of lawful status rather than a structural extraction.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, separated_families, payer,
    powerless, biographical, identity_locked, global).

% Flee persecution or violence but are turned away at borders, denied entry to process asylum claims, or detained in extraterritorial camps. Under the freedom-primary reading, the denial compounds: both the freedom of movement is negated AND the differentiated protection obligation to those fleeing state violence is voided. Their trapped exit reflects literal confinement (detention) and geographic barriers (offshore processing centers, closed ports) alongside the movement denial.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, asylum_seekers, payer,
    powerless, immediate, trapped, universal).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__freedom_of_movement_primary, asylum_seekers, excluded).

% Comprise high-income states that gain material benefit from border closure: protected labor markets, compressed wage floors, and welfare distribution concentrated on incumbents. These jurisdictions have the power to choose closure or opening; their choice for closure reflects power asymmetry (they can afford exclusion) rather than necessity. Under the freedom-primary reading, this benefit is recognized as extraction from the powerless who are denied movement and opportunity.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, receiving_jurisdictions_labor_surplus, beneficiary,
    powerful, generational, mobile, national).

% Theoretically uphold freedom of movement as binding international law (UDHR Article 13, ICCPR Article 12, African Charter) but lack enforcement mechanisms over sovereign states. They can audit state practice, issue verdicts, and name violations, but cannot compel state compliance without state assent. Their observer role exposes the gap between the freedom-primary reading's claimed universality and its actual powerlessness against state enforcement.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, supranational_human_rights_bodies, observer,
    institutional, generational, analytical, universal).

% Actively contest the freedom-primary reading and defend the sovereignty-primary one. They frame border closure as existential (cultural survival, political self-determination) and movement rights as threats to group autonomy. Under the freedom-primary reading, they are excluded from the legitimating conversation because their framing requires denying the basic right the reading asserts. Their exclusion is not institutional (they have political voice) but epistemic: the reading does not dignify sovereignty-primary framing as a co-equal position on the same kernel.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__freedom_of_movement_primary, nationalist_political_movements, excluded,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_control_legitimacy__freedom_of_movement_primary, incumbent_citizens).
narrative_ontology:fixing_cost_class(border_control_legitimacy__freedom_of_movement_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a system for allocating membership, welfare access, and labor-market participation rights by geographic presence within bordered territory. The coordination solves how to distribute collective goods (public education, healthcare, social insurance) and regulate labor supply when resources appear scarce. The coordination function is genuine and necessary; under the freedom-primary reading, it is systematically conflated with and justifies the exclusion function (denying entry), which the reading asserts is separable and unjustified.
% TRANSFER_FUNCTION: Moves material and opportunity value (jobs, social benefits, legal standing, freedom of movement itself) from the excluded and displaced toward incumbent citizens and powerful receiving jurisdictions. The mechanism extracts by negating the movement right, preventing labor arbitrage, keeping wages artificially high in high-income states, and concentrating welfare distribution on members already present. Extraction is enforced by inspection, detention, expulsion, and the denial of legal recourse.
% ABSENT_VOICES: The voices actually excluded are excluded by design: migrants, the globally displaced, and those whose labor would cross borders if legally permitted are structurally absent from border-setting deliberations. Nationalist political movements participate in the debate but are systematically negated by the freedom-primary reading's assertion that the right to movement is non-negotiable — rendering their objections illegitimate rather than unheard. The asymmetry is crucial: migrants cannot speak because they are prevented from arriving; nationalists can speak but their core claim is negated as indefensible.
% DISAPPEARANCE_RATIONALE: If border closure authority were delegitimized and freedom of movement recognized as binding, the world would reorganize dramatically: labor markets would equilibrate globally, migration pressures would shift toward capacity constraints (housing, resources) rather than legal exclusion, welfare states would either collapse their universalism or reconstitute funding and delivery on transnational bases, and political identity would decentralize from nation-state to nested subnational/transnational units. The constraint is not a background fact; its disappearance unmakes current institutional arrangements.
% FOUNDING_PROBLEM: The founding problem stated by the sovereignty-primary reading is state consolidation and the prevention of mass migration destabilizing territorial administration and institutional coherence. The founding problem inverted by the freedom-primary reading is the moral horror of confining populations to zones of deprivation by denying them the right to seek better circumstances elsewhere. Both are narratives about founding; they are incompatible accounts of what the constraint was built to accomplish.
% FOUNDING_PROBLEM_CORROBORATION: The state apparatus and incumbent-citizen publics attest that border closure prevents administrative chaos and protects social cohesion. Supranational human-rights bodies and migration-advocacy communities attest that the founding problem is a retrospective justification for extraction and that the constraint's persistence serves consolidation of power, not any live coordination problem. Economic analysis shows that barriers to migration produce rents (compressed wages in high-income states, high remittance values) benefiting those already positioned in high-income labor markets — a structural fact supporting the freedom-primary reading's extraction interpretation. No single authoritative corroborator exists; the divergence itself is evidence of deep contestation.
narrative_ontology:disappearance_verdict(border_control_legitimacy__freedom_of_movement_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__freedom_of_movement_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__freedom_of_movement_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_control_legitimacy__freedom_of_movement_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__freedom_of_movement_primary, 0.82, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_control_legitimacy__freedom_of_movement_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_control_legitimacy__freedom_of_movement_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_control_legitimacy__freedom_of_movement_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high and rising (0.71 to 0.82 over the interval) because the constraint's operation increasingly prioritizes exclusion-maintenance over genuine coordination: wall-building, surveillance expansion, offshore processing. Suppression is highest (0.87) because the constraint depends entirely on active coercive enforcement — preventing movement is not a natural outcome but requires continuous detention, expulsion, and border patrol machinery. Theater ratio rises from 0.35 to 0.48, indicating growing performative messaging ('border security,' 'sovereignty') alongside the extraction itself. Accessibility collapse is moderate (0.71) because alternatives are not entirely foreclosed — one can legally stay within origin country or seek asylum (theoretically) — but the practical alternatives narrow as enforcement hardens. Resistance is substantial (0.64): migration movements, advocacy organizations, judicial resistance, international pressure all mount active opposition. The measurement series traces 8 time points on one shared grid (every metric at every point). Theater ratio plateaus after t=25 because the performative messaging saturates; suppression continues rising because enforcement infrastructure hardens (biometric systems, detention expansion). The rising extractiveness-to-theater gap (0.82 vs 0.48) indicates a constraint increasingly frank about its extraction function rather than clothed in coordination cover — a degradation in the reading's own legitimacy narrative.
 *
 * PERSPECTIVAL GAP:
 *   Under the freedom-primary reading, the agenda-setter (state apparatus) and beneficiary seats (incumbent citizens) experience the constraint as necessary coordination and protection; payer and excluded seats experience it as denial of a fundamental right and systematic extraction. This perspectival gap is structural, not observational: the state sees order; the powerless see confinement. The engine computes directionality from these role declarations, not from abstract fairness. An agenda-setter with institutional power and arbitrage-grade exit (the state apparatus CAN choose to open borders) will derive low d → low χ (subsidy effect) in the engine, because the apparatus is not trapped. Incumbents with organized power and mobile exit derive similarly low d. Powerless victims with trapped or identity-locked exit derive high d → high χ. The same constraint produces radically different type classifications per seat.
 *
 * DIRECTIONALITY LOGIC:
 *   The state apparatus (institutional power, arbitrage exit) benefits from administering the constraint and maintains it despite international pressure — it derives legitimacy from sovereignty claims even as those claims erode under the freedom-primary reading. It is a beneficiary to its own enforcement. Incumbent citizens (organized power, mobile exit) benefit from labor-market protection and welfare priority; they have the power to leave if closure ended, so their exit is mobile and their directionality low. Excluded migrants (powerless, trapped exit) are the structural targets: they cannot move, cannot appeal, cannot change the constraint's operation from within it. Their d is near 1.0 (full target). Displaced workers and separated families occupy the same power level (powerless) but have identity-locked exit: they could legally immigrate to a third country but cannot psychologically abandon the attempt to reach the specific jurisdiction where family or livelihood exists, or they internalize the exclusion as natural. Identity lock increases the suppressive force of the extraction because the agent carries it internally. Asylum seekers have trapped exit (no legal status allows them to stay and work in most jurisdictions, so they are trapped in irregular status or camps). The directionality derivation chain produces: beneficiaries near 0.0, moderate incumbents near 0.4–0.5, powerless payers near 0.85–0.95 (depending on exit type).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem under the freedom-primary reading is framed as the moral necessity of human mobility against artificial geographic confinement. The founding problem under the sovereignty-primary reading is framed as state administrative coherence against migration destabilization. These are incompatible framings of the SAME kernel. The constraint's mandatrophy is located in the sovereignty-primary reading's use of the 'founding problem solved' narrative to justify exclusion: that narrative rests on the claim that border closure prevents chaos. However, the freedom-primary reading asserts that no such chaos justifies denying movement, and that the 'foundational' problem the constraint actually solves is the protection of incumbent-citizen rents from global labor-market competition. Under this reading, the constraint has resolved its stated mandate (admin coherence) through an unjustified means (movement denial) and has replaced that mandate with a de facto mandate to maintain extraction. This is mandatrophy in the classical sense: the constraint persists because it benefits identifiable parties, not because the problem it was built to solve remains live. Declaring this mandatrophy is possible only from the freedom-primary reading; the sovereignty-primary reading would deny it. The reading-indexed nature of mandatrophy is essential: it does not exist objectively, it exists relative to which fundamental right or value is treated as non-negotiable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    freedom_movement_universality_boundary,
    'Does freedom of movement extend universally to all persons regardless of origin, or only to citizens and those with legitimate visa status?',
    'International legal evolution: testing whether binding protocols (UDHR Article 13, ICCPR Article 12) are enforced against states, or whether enforcement remains contingent on bilateral agreement and state consent.',
    'If enforced universally, the state apparatus loses legitimacy for exclusion and the constraint reclassifies toward pure extraction (snare). If bounded to citizens/legal entrants, the constraint remains tangled_rope with the boundary itself becoming a new subject of contestation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(freedom_movement_universality_boundary, empirical, 'The scope and enforceability of freedom-of-movement claims.').

omega_variable(
    coordination_extraction_separability,
    'Can the genuine coordination function (allocating membership and welfare access) be decoupled from the extraction function (denying movement to prevent labor-market equilibration)?',
    'Natural experiment from open-migration jurisdictions (Schengen, Nordic freedom of movement): evidence of stable coordination mechanisms and welfare systems operating without border-based exclusion.',
    'If separable, border closure is pure extraction with a coordination cover story, validating the freedom-primary reading''s mandate to delegitimize the exclusion mechanism while preserving the regulatory function. If inseparable, the coordination genuinely depends on closure and the extraction is a necessary cost, supporting compromise positions (managed migration, work quotas) over the freedom-primary claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether coordination and extraction functions are structurally separable.').

omega_variable(
    identity_lock_internalization_suppression,
    'What portion of the measured suppression reflects structural barriers (legal exclusion, physical walls, detention) versus internalized belief that movement-denial is justified or inevitable?',
    'Post-escape suppression trajectory: if displaced workers and separated families experience reduced suppression after crossing borders or gaining legal status, the suppression was partly internalized; if suppression persists in new contexts (imposter syndrome, expecting re-expulsion), the internalization is deeper.',
    'If primarily structural, the constraint''s power can be broken through legal/institutional change. If internalization is substantial, the constraint carries its suppressive force with the agent after escape, indicating a deeper cognitive colonization mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_internalization_suppression, empirical, 'Suppression mechanism: structural versus internalized.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'Does the freedom-primary reading logically foreclose the sovereignty-primary reading, or do both remain live positions held by different political and epistemic communities?',
    'Philosophical analysis of the core premises: if freedom of movement is asserted as non-negotiable universal right, does that logically contradict the claim that state discretion is absolute? Or can both be held by different communities with different foundational values?',
    'If foreclosed, the sovereignty-primary reading is structurally incoherent and its persistence indicates false consciousness or power-driven denial. If coexisting, the kernel remains genuinely contested and no universal reading is possible without authoritarian imposition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether the reading''s core claim forecloses or coexists with sovereignty-primary framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__freedom_of_movement_primary, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(bord_tr_t0, observed).
narrative_ontology:measurement(bord_tr_t5, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 5, 0.38).
narrative_ontology:measurement_basis(bord_tr_t5, observed).
narrative_ontology:measurement(bord_tr_t10, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 10, 0.41).
narrative_ontology:measurement_basis(bord_tr_t10, observed).
narrative_ontology:measurement(bord_tr_t15, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 15, 0.44).
narrative_ontology:measurement_basis(bord_tr_t15, observed).
narrative_ontology:measurement(bord_tr_t20, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 20, 0.46).
narrative_ontology:measurement_basis(bord_tr_t20, observed).
narrative_ontology:measurement(bord_tr_t25, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 25, 0.48).
narrative_ontology:measurement_basis(bord_tr_t25, observed).
narrative_ontology:measurement(bord_tr_t30, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 30, 0.48).
narrative_ontology:measurement_basis(bord_tr_t30, observed).
narrative_ontology:measurement(bord_tr_t40, border_control_legitimacy__freedom_of_movement_primary, theater_ratio, 40, 0.48).
narrative_ontology:measurement_basis(bord_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 0, 0.71).
narrative_ontology:measurement_basis(bord_be_t0, observed).
narrative_ontology:measurement(bord_be_t5, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 5, 0.74).
narrative_ontology:measurement_basis(bord_be_t5, observed).
narrative_ontology:measurement(bord_be_t10, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 10, 0.76).
narrative_ontology:measurement_basis(bord_be_t10, observed).
narrative_ontology:measurement(bord_be_t15, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 15, 0.79).
narrative_ontology:measurement_basis(bord_be_t15, observed).
narrative_ontology:measurement(bord_be_t20, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 20, 0.81).
narrative_ontology:measurement_basis(bord_be_t20, observed).
narrative_ontology:measurement(bord_be_t25, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 25, 0.82).
narrative_ontology:measurement_basis(bord_be_t25, observed).
narrative_ontology:measurement(bord_be_t30, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 30, 0.82).
narrative_ontology:measurement_basis(bord_be_t30, observed).
narrative_ontology:measurement(bord_be_t40, border_control_legitimacy__freedom_of_movement_primary, base_extractiveness, 40, 0.82).
narrative_ontology:measurement_basis(bord_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 0, 0.78).
narrative_ontology:measurement_basis(bord_su_t0, observed).
narrative_ontology:measurement(bord_su_t5, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 5, 0.8).
narrative_ontology:measurement_basis(bord_su_t5, observed).
narrative_ontology:measurement(bord_su_t10, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 10, 0.82).
narrative_ontology:measurement_basis(bord_su_t10, observed).
narrative_ontology:measurement(bord_su_t15, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 15, 0.84).
narrative_ontology:measurement_basis(bord_su_t15, observed).
narrative_ontology:measurement(bord_su_t20, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 20, 0.86).
narrative_ontology:measurement_basis(bord_su_t20, observed).
narrative_ontology:measurement(bord_su_t25, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 25, 0.87).
narrative_ontology:measurement_basis(bord_su_t25, observed).
narrative_ontology:measurement(bord_su_t30, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 30, 0.87).
narrative_ontology:measurement_basis(bord_su_t30, observed).
narrative_ontology:measurement(bord_su_t40, border_control_legitimacy__freedom_of_movement_primary, suppression_requirement, 40, 0.87).
narrative_ontology:measurement_basis(bord_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__freedom_of_movement_primary, resource_allocation).
narrative_ontology:boltzmann_floor_override(border_control_legitimacy__freedom_of_movement_primary, 0.18).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy__jurisdictional_sovereignty).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, border_control_legitimacy__sovereignty_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, refugee_protection_obligation).
narrative_ontology:affects_constraint(border_control_legitimacy__freedom_of_movement_primary, labor_market_exclusion_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is part of the border_control_legitimacy kernel family. The three readings (freedom-movement-primary, jurisdictional-sovereignty, sovereignty-primary) share the same kernel (border authority legitimacy) but produce different ε values, different beneficiary/victim sets, and different mandatrophy narratives. The freedom-movement-primary reading (this story) treats exclusion as fundamentally unjustified; the sovereignty-primary reading treats it as constitutive of statehood; the jurisdictional-sovereignty reading attempts a middle path recognizing both values. The three stories are linked via network.affects_constraints showing the family relationship and the direction of structural influence (freedom-primary reading challenges the authority legitimacy the sovereignty-primary reading depends on).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_control_legitimacy__freedom_of_movement_primary, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
