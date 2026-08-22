% ============================================================================
% CONSTRAINT STORY: border_control_legitimacy__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: border_control_legitimacy__sovereignty_primary
 *   human_readable: State Territorial Sovereignty and Border Control Discretion (Sovereignty-Primary Reading)
 *   domain: political/legal/migration
 *
 * SUMMARY:
 *   The sovereignty-primary reading of border control legitimacy treats state
 *   territorial sovereignty as entailing absolute discretion to exclude
 *   non-citizens; border control is framed as constitutive of statehood
 *   itself. Under this reading, a state without the power to control its
 *   borders lacks the foundational capacity to exercise sovereignty. This
 *   constraint is ONE READING of a three-way contested kernel
 *   (border_control_legitimacy). The kernel contest includes:
 *   freedom_of_movement_primary (freedom of movement is a fundamental human
 *   right, not overridable by sovereignty claims); jurisdictional_sovereignty
 *   (sovereignty is authority to regulate within territory but does not
 *   necessarily include border closure authority); and this reading,
 *   sovereignty_primary. The constraint story here instantiates ONLY the
 *   sovereignty-primary reading—describing its structural beneficiaries,
 *   victims, enforcement mechanisms, and temporal profile. The three readings
 *   are separate constraint stories, linked via network.affects_constraints.
 *   This reading characterizes border enforcement as a tangled_rope: it
 *   performs a genuine coordination function (defining a bounded political
 *   community) AND operates as asymmetric extraction (migrants bear the cost
 *   of exclusion). The claim/metric gap is deliberate and structural:
 *   sovereignty theorists claim rope (coordination), but the authored metrics
 *   describe substantially extractive, actively enforced operation with
 *   mounting theater ratio (growing performative maintenance of the 'absolute
 *   discretion' claim against pressure from alternative readings).
 *
 * KEY AGENTS:
 *   - state_administrative_apparatus: controls border enforcement, exercises claimed absolute discretion, benefits from authority collection
 *   - citizen_polity: organized constituency receiving membership scarcity benefit, identity-locked to state inclusion
 *   - excluded_migrants: powerless, trapped targets of the extraction; their exclusion is the constraint's enforcement object
 *   - asylum_seekers: liminal status; their presence at the border contests the absolute discretion claim; highest suppression load
 *   - alternative_reading_proponents: human rights advocates, post-colonial scholars; excluded from enforcement authority but mounted advocacy
 *   - competing_states: institutional observers; recognize each other's sovereignty-primary reading as international norm
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_control_legitimacy__sovereignty_primary, 0.81).
domain_priors:suppression_score(border_control_legitimacy__sovereignty_primary, 0.79).
domain_priors:theater_ratio(border_control_legitimacy__sovereignty_primary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, extractiveness, 0.81).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, accessibility_collapse, 0.73).
narrative_ontology:constraint_metric(border_control_legitimacy__sovereignty_primary, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_control_legitimacy__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(border_control_legitimacy__sovereignty_primary, "State Territorial Sovereignty and Border Control Discretion (Sovereignty-Primary Reading)").
narrative_ontology:topic_domain(border_control_legitimacy__sovereignty_primary, "political/legal/migration").

domain_priors:requires_active_enforcement(border_control_legitimacy__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_control_legitimacy__sovereignty_primary, '95192c86-7fe5-4589-8d44-a1361e03bec6').
narrative_ontology:cs_kernel_codification('95192c86-7fe5-4589-8d44-a1361e03bec6', formalized).
narrative_ontology:cs_authority_grounding('95192c86-7fe5-4589-8d44-a1361e03bec6', extraction).
narrative_ontology:cs_interpretation_layer_present('95192c86-7fe5-4589-8d44-a1361e03bec6').
narrative_ontology:cs_reading_relation('95192c86-7fe5-4589-8d44-a1361e03bec6', border_control_legitimacy__freedom_of_movement_primary, forecloses).
narrative_ontology:cs_reading_relation('95192c86-7fe5-4589-8d44-a1361e03bec6', border_control_legitimacy__jurisdictional_sovereignty, influences).
narrative_ontology:cs_axiom('95192c86-7fe5-4589-8d44-a1361e03bec6', foundational, state_absolute_border_discretion).
narrative_ontology:cs_axiom_status(state_absolute_border_discretion, holdable).
narrative_ontology:cs_axiom_grounding('95192c86-7fe5-4589-8d44-a1361e03bec6', state_absolute_border_discretion, deontological).
narrative_ontology:cs_axiom('95192c86-7fe5-4589-8d44-a1361e03bec6', foundational, border_control_constitutive_statehood).
narrative_ontology:cs_axiom_status(border_control_constitutive_statehood, holdable).
narrative_ontology:cs_axiom_grounding('95192c86-7fe5-4589-8d44-a1361e03bec6', border_control_constitutive_statehood, deontological).
narrative_ontology:cs_reference_frame('95192c86-7fe5-4589-8d44-a1361e03bec6', sovereign_exclusive_border_authority).
narrative_ontology:cs_drift_state('95192c86-7fe5-4589-8d44-a1361e03bec6', contemporary_human_rights_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('95192c86-7fe5-4589-8d44-a1361e03bec6', '').
narrative_ontology:cs_kernel_id(border_control_legitimacy__sovereignty_primary, border_control_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, state_administrative_apparatus).
narrative_ontology:constraint_beneficiary(border_control_legitimacy__sovereignty_primary, citizen_polity).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, excluded_migrants).
narrative_ontology:constraint_victim(border_control_legitimacy__sovereignty_primary, asylum_seekers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Exercises absolute discretion over border control, immigration law, and migrant exclusion. Justifies this authority as constitutive of sovereignty itself — the capacity to determine who belongs and who does not. Operates border enforcement machinery, visa systems, and deportation procedures. Collects legitimacy from the claim that territorial control is the foundational act of statehood.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, state_administrative_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Treated as the primary constituency whose interests are served by border control. Receives the benefit of a bounded, controllable polity; membership is scarce and conferring it is framed as a state prerogative. Labor competition and resource allocation are mediated through membership control. Exit from this role is constitutive identity-fusion — one does not become 'not-citizen' without relocating entirely.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, citizen_polity, beneficiary,
    organized, generational, identity_locked, national).

% Bear the primary cost of border exclusion: denied access to territory, labor markets, public services, and legal status. Lack formal voice in the polity that excludes them. Their exclusion is the enforcement object itself — the constraint persists by keeping them out. Deportation, detention, and legal barriers to entry are the enforcement mechanisms. They cannot exit this constraint by accepting worse terms; exit means physical relocation to a different state's border.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, excluded_migrants, payer,
    powerless, biographical, trapped, global).

% Occupy a liminal status: present at or near the border, claiming protection grounds that challenge the sovereignty-primary reading (persecution, warfare, survival necessity). They are treated as boundary cases whose inclusion would dilute the state's claimed absolute discretion. Detention, legal processing, and summary exclusion are applied to them. They carry the highest suppression burden because their very presence at the border contests the absolute discretion claim.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, asylum_seekers, payer,
    powerless, immediate, trapped, global).
narrative_ontology:stakeholder_secondary_role(border_control_legitimacy__sovereignty_primary, asylum_seekers, excluded).

% Human rights advocates, migration scholars, freedom-of-movement proponents, and some indigenous/post-colonial governments hold alternative readings (freedom_of_movement_primary, jurisdictional_sovereignty) that contest the absolute discretion claim. They are excluded from formal power over border enforcement but conduct advocacy, litigation, and norm-setting that creates structural pressure on the sovereignty-primary reading. They would argue that human rights protections are constitutive of legitimate authority, not external limits on it.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, alternative_reading_proponents, excluded,
    moderate, generational, analytical, global).

% Each state exercises its own claimed absolute discretion over borders; they recognize each other's sovereignty-primary reading as the operative international norm, even where individual states adopt humanitarian or open-migration policies domestically. International law (1951 Refugee Convention, human rights treaties) creates external constraint on the reading's practical scope, but does not challenge its foundational claim to authority.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, competing_states, observer,
    institutional, generational, analytical, global).

% Maps the constraint's structure: how the reading distributes benefits (state control, citizen membership scarcity, administrative prerogative) and costs (migrant exclusion, asylum denial, immobility). Identifies the reading as one among three structurally distinct framings of the same contested kernel.
narrative_ontology:constraint_stakeholder(border_control_legitimacy__sovereignty_primary, analytical_observer, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_control_legitimacy__sovereignty_primary, state_administrative_apparatus).
narrative_ontology:fixing_cost_class(border_control_legitimacy__sovereignty_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a bounded political community with defined membership and exclusive authority over entry. Solves the coordination problem of how to form a self-governing polity when movement across borders is physically possible — by asserting the state's power to control boundaries and determine who participates in collective decision-making.
% TRANSFER_FUNCTION: Transfers the benefit of membership scarcity and political inclusion to citizens, while transferring the cost of exclusion and immobility to migrants. The state apparatus collects the authority and legitimacy to make this transfer; excluded migrants bear the deprivation. Labor market access and public goods are distributed along the membership boundary.
% ABSENT_VOICES: Excluded migrants themselves have no formal voice in the border-setting process — they are the objects of the constraint, not participants in setting it. Alternative reading proponents (human rights advocates, freedom-of-movement theorists) are excluded from enforcement authority but conduct advocacy outside. Migrants in origin countries who never attempt entry are invisible to the constraint's formal decision-making.
% DISAPPEARANCE_RATIONALE: If state absolute discretion over borders disappeared and were replaced by freedom-of-movement norms, the international system would reorganize: labor markets would open, migration flows would surge, citizenship would lose scarcity value, and polities would need to renegotiate membership criteria on non-exclusionary grounds. The state apparatus would lose a primary legitimating authority (border control), forcing redistribution of political power and resource allocation.
% FOUNDING_PROBLEM: How to establish a self-governing political community when movement is physically possible and identities are not naturally bounded. The founding problem under this reading: sovereignty requires the power to control membership; without border discretion, the state has no basis to claim exclusive authority over its citizens or territory.
% FOUNDING_PROBLEM_CORROBORATION: State actors and sovereignty theorists (Jean Bodin, Thomas Hobbes tradition, contemporary international law) attest the founding problem is live and unsolved without border control. Human rights frameworks and freedom-of-movement theorists attest the problem is falsely posed — that a legitimate state can be defined by rights protections and consensual governance, not by exclusionary membership control. Post-colonial scholars attest the problem itself is a European export that denies indigenous and non-Western governance models. The attestations split along reading lines; no neutral external corroboration exists.
narrative_ontology:disappearance_verdict(border_control_legitimacy__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_control_legitimacy__sovereignty_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_control_legitimacy__sovereignty_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_control_legitimacy__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_control_legitimacy__sovereignty_primary, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.68 to 0.81 over the interval (observed), indicating accumulating enforcement pressure as alternative readings gain advocacy salience. The state responds by intensifying border mechanisms (deportation, detention, legal barriers) to defend the 'absolute discretion' claim. Theater_ratio rises from 0.28 to 0.42, signaling growing performative maintenance: as human rights frameworks and freedom-of-movement norms exert structural pressure, more of the enforcement apparatus shifts from functional security review (founding problem: bounded polity definition) to theatrical sovereignty defense (defending the reading itself against contestation). Suppression_requirement rises from 0.62 to 0.79, indicating the constraint's enforcement increasingly depends on active coercive machinery rather than on voluntary compliance or internalized norms. Accessibility_collapse is high (0.73 at end) because once the sovereignty-primary reading is institutionalized, legal and physical alternatives to exclusion collapse nearly completely for migrants—they cannot negotiate membership, cannot appeal to overriding human rights, and cannot move freely. Resistance is substantial (0.68 at interval end) because excluded migrants, asylum advocates, and alternative-reading proponents mount real opposition to the constraint, though they lack formal power to override it.
 *
 * PERSPECTIVAL GAP:
 *   The seat divergence is severe and structural. From the state apparatus's seat, the constraint is justified coordinate function—rational boundary-setting for a self-governing polity (rope framing). From the excluded-migrant seat, the same structure operates as extraction—deprivation of mobility, labor access, and legal status with no exit available except relocation to a different state's border (snare framing). From the citizen-polity seat, the constraint is a beneficent membership boundary that confers scarcity and political voice (beneficiary framing). From the asylum-seeker seat, it is immediate survival threat layered with legal imprisonment (snare+entrapment framing). The engine computes per-seat classification from the structural data: directionality toward the state apparatus is low (beneficiary d ~0.2), directionality toward excluded migrants is high (target d ~0.95), directionality toward citizens is mixed (both benefit from scarcity and bear indirect migration-cost inflation, d ~0.55). The alternative-reading-proponent seat has d ~0.6 (organizational power, excluded from enforcement but conducting structural pressure).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations anchor the directionality derivation: state apparatus benefits from authority collection and legitimacy defense; citizen polity benefits from membership scarcity and political inclusion. Victim declarations anchor the other direction: excluded migrants and asylum seekers are targets whose exclusion IS the enforcement object—they cannot negotiate exit, cannot accept worse terms (there are none available), and are trapped by global state system structure (no non-state territory to move to). Exit_options differentiate the seats: state_administrative_apparatus has analytical exit (can adopt alternative readings but chooses not to), citizen_polity has identity_locked exit (one does not become 'not-citizen' without full relocation, a constitutive identity breakage), excluded_migrants have trapped exit (no lawful path to membership, mobility, or legal status). Power levels cascade: institutional (state) → organized (citizen polity) → powerless (migrants). Effective directionality (χ) is amplified for trapped targets (migrants, d→1.0) and damped for identity-locked beneficiaries (citizens, d→0.5 for mixed extraction/benefit). The state apparatus sits near d=0.25 (powerful institutional beneficiary with substantial exit—they could change the policy, but choose not to).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint shows incipient mandatrophy: the founding problem (how to bound a political community) is partly solved by border control, but the measurement series indicates the enforcement apparatus is increasingly devoted to defending the reading itself against contestation rather than to solving the founding problem. Theater_ratio rising from 0.28 to 0.42 (43% of enforcement effort becoming performative sovereignty defense) is the key signal. The constraint persists because no alternative reading has achieved sufficient institutional power to dislodge it, and because citizens benefit from membership scarcity; payers (migrants, asylum seekers) lack exit and formal voice. The finding is NOT yet mandatrophy (base_extractiveness stabilizes at 0.81, not rising toward 1.0 with all function decayed), but the trajectory is concerning: if theater_ratio continues to rise while coordination function stays flat, the constraint could degrade from tangled_rope to piton (mostly performance, no real beneficiary maintaining it except the apparatus itself). The mandatrophy risk resolution mechanism: if freedom-of-movement norms achieve regional legal binding (e.g., through treaty or supranational court), the constraint's legitimacy would collapse and mandatrophy would resolve as 'terminal' (the reading is abandoned). If alternative readings remain excluded from enforcement authority and advocacy fades, mandatrophy persists as incipient (the threat hangs unresolved).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutional_necessity_vs_choice,
    'Is border control authority logically necessary for statehood, or is it a policy choice that states make?',
    'Examine historical and contemporary states that have open or permissive borders within regional agreements (EU Schengen, Caribbean mobility agreements) and assess whether they retain statehood status and sovereignty legitimacy. Test whether losing border control authority is experienced as loss of statehood or as policy reorientation.',
    'If border control is logically necessary for statehood, the sovereignty-primary reading''s foundational axiom holds and the constraint''s legitimacy claim is robust. If open borders are compatible with statehood, the axiom is contingent on policy choice, not structural necessity, and alternative readings (freedom_of_movement_primary, jurisdictional_sovereignty) become more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_necessity_vs_choice, conceptual, 'Whether border authority is constitutive of statehood or a contingent policy.').

omega_variable(
    suppression_structural_vs_internalized,
    'For excluded migrants, is the measured suppression (0.83-0.85 at individual level) primarily structural (legal barriers, enforcement machinery, geographic isolation) or partially internalized (belief that exclusion is legitimate, identity fusion with exclusion as ''natural'')?',
    'Compare suppression persistence among: (a) migrants detained and released with legal status versus (b) migrants in asylum limbo; (c) migrants post-entry (if they successfully overcome borders) versus pre-entry. If suppression persists post-exit (e.g., post-entry migrants still see movement as illegitimate), it is partially internalized; if suppression collapses post-entry, it is structural.',
    'If suppression is structural, the constraint''s effective extraction remains high because the barriers are external and persistent. If suppression is internalized, the constraint''s effective extraction is higher than the structural measure suggests—the target carries the suppression with them even after legal status change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Suppression mechanism: structural barriers versus internalized legitimacy belief.').

omega_variable(
    founding_problem_persistence,
    'The founding problem stated under this reading is ''how to establish a self-governing political community when movement is physically possible?'' Is this founding problem still live, or has it been partly solved and now serves as cover for rent collection?',
    'Examine whether border control effort correlates with actual security/polity-definition challenges (refugee flows, labor migration patterns, asymmetric-information problems in membership) or with political cycle dynamics, anti-immigrant sentiment, and resource competition unrelated to the founding problem''s technical solution.',
    'If the founding problem is live, the constraint retains its tangled_rope character (coordination + extraction). If the problem is solved but border control persists as a tool for membership scarcity and labor market control, the constraint drifts toward snare (extraction disguised as coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the founding problem (self-governing community definition) is still the active driver of border policy.').

omega_variable(
    kernel_reading_coexistence,
    'Can the sovereignty_primary reading and freedom_of_movement_primary reading coexist within a single state''s legal framework, or are they logically foreclosed to each other?',
    'Test through comparative law: examine states that recognize freedom of movement as fundamental right AND maintain strong border control authority. Assess whether they do so through interpretive compartmentalization (movement within territory / movement across borders as separate domains) or whether the readings are in genuine tension that forces policy choice.',
    'If readings can coexist through compartmentalization, they are merely coexisting alternatives, not foreclosed. If they force a policy choice, the reading relation should be ''forecloses'' rather than ''coexists_with'', and the constraint''s legitimacy claim is more directly contested.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_coexistence, conceptual, 'Logical relationship between sovereignty-primary and freedom-of-movement-primary readings.').

omega_variable(
    international_norm_stability,
    'Is the sovereignty-primary reading stable as an international norm, or is it being eroded by human rights frameworks, climate migration, and supranational authority?',
    'Track binding treaty obligations (refugee conventions, human rights courts'' jurisprudence, regional open-border agreements) over time. Measure state-level policy drift toward more permissive migration. Assess whether states claiming absolute border discretion face institutional costs (sanctions, international court losses, legitimacy erosion).',
    'If the reading is stable, the constraint should maintain high extractiveness and legitimacy. If the reading is eroding, theater_ratio should continue rising (more performative sovereignty defense), and alternative readings should gain enforcement authority, eventually displacing this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_norm_stability, empirical, 'Stability of sovereignty-primary reading in international law and state practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_control_legitimacy__sovereignty_primary, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t0, border_control_legitimacy__sovereignty_primary, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(bord_tr_t0, observed).
narrative_ontology:measurement(bord_tr_t10, border_control_legitimacy__sovereignty_primary, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(bord_tr_t10, observed).
narrative_ontology:measurement(bord_tr_t20, border_control_legitimacy__sovereignty_primary, theater_ratio, 20, 0.35).
narrative_ontology:measurement_basis(bord_tr_t20, observed).
narrative_ontology:measurement(bord_tr_t30, border_control_legitimacy__sovereignty_primary, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(bord_tr_t30, observed).
narrative_ontology:measurement(bord_tr_t40, border_control_legitimacy__sovereignty_primary, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(bord_tr_t40, observed).
narrative_ontology:measurement(bord_tr_t50, border_control_legitimacy__sovereignty_primary, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(bord_tr_t50, observed).
narrative_ontology:measurement(bord_tr_t60, border_control_legitimacy__sovereignty_primary, theater_ratio, 60, 0.42).
narrative_ontology:measurement_basis(bord_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(bord_be_t0, border_control_legitimacy__sovereignty_primary, base_extractiveness, 0, 0.68).
narrative_ontology:measurement_basis(bord_be_t0, observed).
narrative_ontology:measurement(bord_be_t10, border_control_legitimacy__sovereignty_primary, base_extractiveness, 10, 0.71).
narrative_ontology:measurement_basis(bord_be_t10, observed).
narrative_ontology:measurement(bord_be_t20, border_control_legitimacy__sovereignty_primary, base_extractiveness, 20, 0.75).
narrative_ontology:measurement_basis(bord_be_t20, observed).
narrative_ontology:measurement(bord_be_t30, border_control_legitimacy__sovereignty_primary, base_extractiveness, 30, 0.77).
narrative_ontology:measurement_basis(bord_be_t30, observed).
narrative_ontology:measurement(bord_be_t40, border_control_legitimacy__sovereignty_primary, base_extractiveness, 40, 0.79).
narrative_ontology:measurement_basis(bord_be_t40, observed).
narrative_ontology:measurement(bord_be_t50, border_control_legitimacy__sovereignty_primary, base_extractiveness, 50, 0.81).
narrative_ontology:measurement_basis(bord_be_t50, observed).
narrative_ontology:measurement(bord_be_t60, border_control_legitimacy__sovereignty_primary, base_extractiveness, 60, 0.81).
narrative_ontology:measurement_basis(bord_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t0, border_control_legitimacy__sovereignty_primary, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(bord_su_t0, observed).
narrative_ontology:measurement(bord_su_t10, border_control_legitimacy__sovereignty_primary, suppression_requirement, 10, 0.66).
narrative_ontology:measurement_basis(bord_su_t10, observed).
narrative_ontology:measurement(bord_su_t20, border_control_legitimacy__sovereignty_primary, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(bord_su_t20, observed).
narrative_ontology:measurement(bord_su_t30, border_control_legitimacy__sovereignty_primary, suppression_requirement, 30, 0.73).
narrative_ontology:measurement_basis(bord_su_t30, observed).
narrative_ontology:measurement(bord_su_t40, border_control_legitimacy__sovereignty_primary, suppression_requirement, 40, 0.76).
narrative_ontology:measurement_basis(bord_su_t40, observed).
narrative_ontology:measurement(bord_su_t50, border_control_legitimacy__sovereignty_primary, suppression_requirement, 50, 0.78).
narrative_ontology:measurement_basis(bord_su_t50, observed).
narrative_ontology:measurement(bord_su_t60, border_control_legitimacy__sovereignty_primary, suppression_requirement, 60, 0.79).
narrative_ontology:measurement_basis(bord_su_t60, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=60
narrative_ontology:measurement(bord_grid_01, border_control_legitimacy__sovereignty_primary, accessibility_collapse(class), 0, 0.72).
narrative_ontology:measurement(bord_grid_02, border_control_legitimacy__sovereignty_primary, accessibility_collapse(class), 60, 0.75).
narrative_ontology:measurement(bord_grid_03, border_control_legitimacy__sovereignty_primary, accessibility_collapse(individual), 0, 0.75).
narrative_ontology:measurement(bord_grid_04, border_control_legitimacy__sovereignty_primary, accessibility_collapse(individual), 60, 0.77).
narrative_ontology:measurement(bord_grid_05, border_control_legitimacy__sovereignty_primary, accessibility_collapse(organizational), 0, 0.68).
narrative_ontology:measurement(bord_grid_06, border_control_legitimacy__sovereignty_primary, accessibility_collapse(organizational), 60, 0.71).
narrative_ontology:measurement(bord_grid_07, border_control_legitimacy__sovereignty_primary, accessibility_collapse(structural), 0, 0.71).
narrative_ontology:measurement(bord_grid_08, border_control_legitimacy__sovereignty_primary, accessibility_collapse(structural), 60, 0.73).
narrative_ontology:measurement(bord_grid_09, border_control_legitimacy__sovereignty_primary, resistance(class), 0, 0.68).
narrative_ontology:measurement(bord_grid_10, border_control_legitimacy__sovereignty_primary, resistance(class), 60, 0.71).
narrative_ontology:measurement(bord_grid_11, border_control_legitimacy__sovereignty_primary, resistance(individual), 0, 0.61).
narrative_ontology:measurement(bord_grid_12, border_control_legitimacy__sovereignty_primary, resistance(individual), 60, 0.63).
narrative_ontology:measurement(bord_grid_13, border_control_legitimacy__sovereignty_primary, resistance(organizational), 0, 0.54).
narrative_ontology:measurement(bord_grid_14, border_control_legitimacy__sovereignty_primary, resistance(organizational), 60, 0.58).
narrative_ontology:measurement(bord_grid_15, border_control_legitimacy__sovereignty_primary, resistance(structural), 0, 0.42).
narrative_ontology:measurement(bord_grid_16, border_control_legitimacy__sovereignty_primary, resistance(structural), 60, 0.45).
narrative_ontology:measurement(bord_grid_17, border_control_legitimacy__sovereignty_primary, stakes_inflation(class), 0, 0.78).
narrative_ontology:measurement(bord_grid_18, border_control_legitimacy__sovereignty_primary, stakes_inflation(class), 60, 0.82).
narrative_ontology:measurement(bord_grid_19, border_control_legitimacy__sovereignty_primary, stakes_inflation(individual), 0, 0.86).
narrative_ontology:measurement(bord_grid_20, border_control_legitimacy__sovereignty_primary, stakes_inflation(individual), 60, 0.89).
narrative_ontology:measurement(bord_grid_21, border_control_legitimacy__sovereignty_primary, stakes_inflation(organizational), 0, 0.61).
narrative_ontology:measurement(bord_grid_22, border_control_legitimacy__sovereignty_primary, stakes_inflation(organizational), 60, 0.64).
narrative_ontology:measurement(bord_grid_23, border_control_legitimacy__sovereignty_primary, stakes_inflation(structural), 0, 0.55).
narrative_ontology:measurement(bord_grid_24, border_control_legitimacy__sovereignty_primary, stakes_inflation(structural), 60, 0.58).
narrative_ontology:measurement(bord_grid_25, border_control_legitimacy__sovereignty_primary, suppression(class), 0, 0.79).
narrative_ontology:measurement(bord_grid_26, border_control_legitimacy__sovereignty_primary, suppression(class), 60, 0.82).
narrative_ontology:measurement(bord_grid_27, border_control_legitimacy__sovereignty_primary, suppression(individual), 0, 0.83).
narrative_ontology:measurement(bord_grid_28, border_control_legitimacy__sovereignty_primary, suppression(individual), 60, 0.85).
narrative_ontology:measurement(bord_grid_29, border_control_legitimacy__sovereignty_primary, suppression(organizational), 0, 0.64).
narrative_ontology:measurement(bord_grid_30, border_control_legitimacy__sovereignty_primary, suppression(organizational), 60, 0.68).
narrative_ontology:measurement(bord_grid_31, border_control_legitimacy__sovereignty_primary, suppression(structural), 0, 0.58).
narrative_ontology:measurement(bord_grid_32, border_control_legitimacy__sovereignty_primary, suppression(structural), 60, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_control_legitimacy__sovereignty_primary, identity_coordination).
narrative_ontology:boltzmann_floor_override(border_control_legitimacy__sovereignty_primary, 0.18).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, border_control_legitimacy__freedom_of_movement_primary).
narrative_ontology:affects_constraint(border_control_legitimacy__sovereignty_primary, border_control_legitimacy__jurisdictional_sovereignty).

% DUAL FORMULATION NOTE:
% The border_control_legitimacy kernel decomposes into three structurally distinct readings with different ε values and victim sets. sovereignty_primary (this story) treats absolute border discretion as constitutive of statehood and shows high extractiveness (0.81) from excluded migrants. freedom_of_movement_primary treats movement as a fundamental right overriding sovereignty claims and shows lower extractiveness (different ε, different classification). jurisdictional_sovereignty treats sovereignty as territorial authority without necessary border closure and shows mixed extraction (bounded but not absolute). All three readings reference the same kernel commitment but instantiate different constraints with incompatible beneficiary/victim structures and axioms. Network links enable contamination analysis: pressure from freedom-of-movement norms (measured via alternative-reading-proponent resistance at class/individual levels) drives the rising theater_ratio in sovereignty_primary, indicating the reading's defensive posture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_control_legitimacy__sovereignty_primary, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
