% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__constitutive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__constitutive_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__constitutive_reading
 *   human_readable: Constitutive Recognition Requirement for Statehood
 *   domain: international_law/political_philosophy/state_theory
 *
 * SUMMARY:
 *   The constitutive reading of the Montevideo criteria holds that
 *   recognition by existing states is not merely declaratory of a
 *   pre-existing legal fact but constitutive of statehood itself. An entity
 *   meeting all four objective criteria (permanent population, defined
 *   territory, government, capacity to enter relations) is not a state until
 *   recognized. This reading has been the operational default of the UN
 *   system since 1945: admission requires Security Council recommendation and
 *   General Assembly approval, both political acts. The constraint
 *   coordinates the international system by giving existing states a
 *   collective veto over new state creation, but it extracts asymmetrically —
 *   aspirant polities and their populations bear the costs of non-recognition
 *   while existing states collect the gatekeeping rent. The claim/metric gap
 *   is deliberate: the constraint is claimed as a coordination mechanism
 *   (rope-like) while the authored metrics describe substantial extraction
 *   and active suppression of the declaratory alternative.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__constitutive_reading, 0.72).
domain_priors:suppression_score(montevideo_statehood_criteria__constitutive_reading, 0.78).
domain_priors:theater_ratio(montevideo_statehood_criteria__constitutive_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__constitutive_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__constitutive_reading, "Constitutive Recognition Requirement for Statehood").
narrative_ontology:topic_domain(montevideo_statehood_criteria__constitutive_reading, "international_law/political_philosophy/state_theory").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__constitutive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__constitutive_reading, '439b823c-9af2-44be-976e-251be8aeecd8').
narrative_ontology:cs_kernel_codification('439b823c-9af2-44be-976e-251be8aeecd8', formalized).
narrative_ontology:cs_authority_grounding('439b823c-9af2-44be-976e-251be8aeecd8', practice).
narrative_ontology:cs_interpretation_layer_present('439b823c-9af2-44be-976e-251be8aeecd8').
narrative_ontology:cs_reading_relation('439b823c-9af2-44be-976e-251be8aeecd8', montevideo_statehood_criteria__declaratory_reading, coexists_with).
narrative_ontology:cs_reading_relation('439b823c-9af2-44be-976e-251be8aeecd8', montevideo_statehood_criteria__hybrid_reading, influences).
narrative_ontology:cs_axiom('439b823c-9af2-44be-976e-251be8aeecd8', foundational, recognition_constitutes_statehood).
narrative_ontology:cs_axiom_status(recognition_constitutes_statehood, holdable).
narrative_ontology:cs_axiom_grounding('439b823c-9af2-44be-976e-251be8aeecd8', recognition_constitutes_statehood, conventional).
narrative_ontology:cs_axiom('439b823c-9af2-44be-976e-251be8aeecd8', secondary, existing_states_hold_veto).
narrative_ontology:cs_axiom_status(existing_states_hold_veto, holdable).
narrative_ontology:cs_axiom_grounding('439b823c-9af2-44be-976e-251be8aeecd8', existing_states_hold_veto, conventional).
narrative_ontology:cs_reference_frame('439b823c-9af2-44be-976e-251be8aeecd8', montevideo_conventional_order).
narrative_ontology:cs_drift_state('439b823c-9af2-44be-976e-251be8aeecd8', post_decolonization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('439b823c-9af2-44be-976e-251be8aeecd8', '2026-08-03T14:22:00Z').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, existing_states).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, un_security_council).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, regional_organizations).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, aspirant_polities).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, populations_in_unrecognized_territories).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__constitutive_reading, state_sovereignty_as_legal_construction).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__constitutive_reading, international_community_gatekeeping_function).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collectively control admission to the international legal order through bilateral recognition and multilateral voting. They frame recognition as a political judgment assessing capacity and legitimacy, but exercise it strategically to manage borders, succession, and geopolitical balance. They collect the structural rent of gatekeeping: control over treaty participation, diplomatic immunity, and economic access for new entrants.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, existing_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Serves as the primary multilateral gatekeeper for UN membership, which is the gold standard of recognition. Permanent members wield veto power over admission resolutions, converting the constitutive requirement into a great-power coordination mechanism. They benefit from the legitimacy the recognition filter confers on the UN system while extracting geopolitical concessions from aspirant states.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, un_security_council, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__constitutive_reading, un_security_council, beneficiary).

% Manage regional order by coordinating recognition policies among members (e.g., EU common positions, AU border principles). They benefit from the constitutive frame because it stabilizes regional boundaries and prevents secessionist fragmentation, but their exit from the global recognition regime is constrained by great-power politics and UN Charter obligations.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, regional_organizations, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__constitutive_reading, regional_organizations, agenda_setter).

% Control territory, population, and government (meeting Montevideo's objective criteria) but cannot access treaty regimes, international courts, development finance, or diplomatic protection without recognition. Their exit from the non-recognition trap requires persuading existing states — a process that can take decades and demands political concessions unrelated to state capacity. The constitutive reading makes their statehood contingent on the very actors whose consent they must secure.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, aspirant_polities, payer,
    powerless, biographical, trapped, regional).

% Bear the human costs of non-recognition: no passport recognition, no consular protection, exclusion from international health/education frameworks, barriers to cross-border trade and movement. They have no voice in the recognition process and no exit option — their fate is bound to the aspirant polity's diplomatic success. The constitutive reading renders their rights derivative of a political decision they cannot influence.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, populations_in_unrecognized_territories, payer,
    powerless, generational, trapped, local).

% Produce the doctrinal frameworks (constitutive, declaratory, hybrid) that states and courts cite. They do not collect rents from the recognition system nor pay its costs, but their interpretations shape the legitimacy terrain. The constitutive reading persists partly because it aligns with positivist legal traditions that treat statehood as a status conferred by the international legal order.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(montevideo_statehood_criteria__constitutive_reading, existing_states).
narrative_ontology:fixing_cost_class(montevideo_statehood_criteria__constitutive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages orderly entry into the international system by preventing fragmentation, resolving competing claims over territory, and providing a procedural gate for succession and secession. The recognition requirement coordinates expectations among existing states about which new entities acquire rights and obligations.
% TRANSFER_FUNCTION: Moves control over legal personality, treaty capacity, diplomatic immunity, and access to international institutions from aspirant polities to existing states. Aspirant polities pay with political concessions, territorial compromises, and alignment with great-power interests; existing states collect the gatekeeping rent.
% ABSENT_VOICES: Populations in unrecognized territories (e.g., Somaliland, Taiwan, Transnistria, Nagorno-Karabakh pre-2023) would object to their rights being contingent on recognition they cannot secure. Aspirant polities themselves are structurally excluded from the recognition decision — they are the objects, not subjects, of the constitutive act. Indigenous peoples and stateless nations are excluded from the state-creation conversation entirely.
% DISAPPEARANCE_RATIONALE: If the constitutive recognition requirement vanished overnight, aspirant polities meeting objective criteria would immediately claim treaty rights, ICJ standing, and development finance. Existing states would lose their veto over new state creation. The international legal order would shift from a club model to a status model, triggering a wave of new state claims and forcing renegotiation of border principles, succession rules, and UN membership procedures.
% FOUNDING_PROBLEM: Post-WWI/WWII need to prevent uncontrolled proliferation of state claims, manage dissolution of empires, and provide a stable legal framework for new state admission without encouraging secessionist fragmentation.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the Montevideo Convention's drafting history (1933) and UN Charter Chapter II. However, decolonization scholars (e.g., Crawford, Vidmar) and ICJ opinions (Kosovo Advisory Opinion 2010) attest that the recognition requirement became a tool for great-power management of decolonization and secession, not merely an orderly-admission mechanism. No non-beneficiary corroborates that the original problem persists unchanged.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__constitutive_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__constitutive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__constitutive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__constitutive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__constitutive_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__constitutive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__constitutive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__constitutive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the recognition gate decouples legal personality from objective state capacity, allowing existing states to condition admission on political concessions. Suppression is high (0.78) because the declaratory alternative — that statehood arises automatically from the four criteria — is actively marginalized in UN practice and great-power diplomacy. Theater ratio is moderate (0.48): the recognition process performs legal assessment but often masks geopolitical calculation. Accessibility collapse is high (0.82) for aspirant polities — once the constitutive frame is accepted, no unilateral action can secure statehood. Resistance is moderate (0.55): aspirant polities mount diplomatic campaigns, but structural power asymmetry limits effectiveness.
 *
 * PERSPECTIVAL GAP:
 *   From the existing-state seat, the recognition requirement is a necessary coordination filter preventing chaos. From the aspirant-polity seat, it is an enforced extraction mechanism that makes statehood a privilege granted by the powerful. The engine computes this divergence from the structural data — the authored claim (tangled_rope) acknowledges both coordination and extraction without adjudicating which seat's experience is 'real'.
 *
 * DIRECTIONALITY LOGIC:
 *   Existing states and the UN Security Council are structural beneficiaries (d near 0.0) — they collect the gatekeeping rent, control the agenda, and face no exit pressure. Aspirant polities and their populations are full targets (d near 1.0) — they pay the full extraction cost with trapped exit. Regional organizations sit near symmetric (d ~0.5): they benefit from regional stability but must coordinate with global gatekeepers. International legal scholars are analytical observers (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (managing post-imperial state creation) is substantially resolved — decolonization is largely complete, and the UN has 193 members. Yet the recognition veto persists and has been repurposed for secession management (Kosovo, South Sudan) and great-power competition (Taiwan, Palestine). The arrangement no longer serves its declared coordination function as purely as claimed; it has accumulated extraction layers (political concessions for recognition) that constitute mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutive_vs_declaratory_boundary,
    'Is the constitutive recognition requirement a structurally distinct constraint from the declaratory criteria, or are they two observables of the same state-creation process?',
    'Compare the victim sets and extraction profiles: if aspirant polities meeting all four criteria but denied recognition suffer systematically different outcomes (no treaty access, no ICJ standing) than recognized states, the constraints are structurally distinct. Empirical test: trace outcomes for entities like Somaliland (meets criteria, unrecognized) vs. recognized microstates.',
    'If distinct, the constitutive reading is a separate extractive constraint layered on the declaratory criteria. If the same constraint, the extraction is inherent to statehood itself, not an added layer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutive_vs_declaratory_boundary, conceptual, 'Whether constitutive and declaratory readings instantiate different constraints with different ε values.').

omega_variable(
    recognition_as_coordination_vs_extraction,
    'Does the recognition requirement genuinely coordinate the international system (preventing fragmentation, managing succession), or is the coordination function cover for great-power gatekeeping?',
    'Counterfactual: if recognition were automatic upon meeting objective criteria, would the international system experience more fragmentation, conflict, or instability? Historical test: compare regions with automatic recognition (post-colonial Africa under OAU uti possidetis) vs. regions with contested recognition (Balkans, Caucasus).',
    'If coordination is genuine, the constraint is a tangled_rope with real coordination function. If coordination is pretext, it is a snare with cover story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(recognition_as_coordination_vs_extraction, preference, 'Whether the coordination function is structurally real or a legitimating narrative.').

omega_variable(
    kernel_reading_framing_underdetermination,
    'Does the Montevideo Convention itself constitute the kernel, or is the kernel the broader statehood concept that the Convention merely codified? The framing changes which readings are siblings and what counts as drift.',
    'Trace the genealogy: if the kernel is the Convention text (1933), the constitutive reading is one interpretation of Article 3. If the kernel is the statehood concept (pre-1933 customary law), the Convention is an early codification attempt and the readings are later interpretive traditions. The authority_grounding and drift_state differ accordingly.',
    'If kernel = Convention text, drift_state measures departure from 1933 text. If kernel = statehood concept, drift_state measures departure from customary law. The CS classification (formalized vs. distributed) and reference_frame change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing_underdetermination, conceptual, 'Framing under-determination: what is the stabilized commitment that the authority structure grounds itself in?').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__constitutive_reading, 1933, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(montevideo_constitutive_tr_t1933, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 1933, 0.25).
narrative_ontology:measurement(montevideo_constitutive_tr_t1945, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 1945, 0.32).
narrative_ontology:measurement(montevideo_constitutive_tr_t1960, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 1960, 0.45).
narrative_ontology:measurement(montevideo_constitutive_tr_t1991, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 1991, 0.52).
narrative_ontology:measurement(montevideo_constitutive_tr_t2008, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 2008, 0.49).
narrative_ontology:measurement(montevideo_constitutive_tr_t2024, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 2024, 0.48).

% Extraction over time
narrative_ontology:measurement(montevideo_constitutive_be_t1933, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 1933, 0.45).
narrative_ontology:measurement(montevideo_constitutive_be_t1945, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 1945, 0.52).
narrative_ontology:measurement(montevideo_constitutive_be_t1960, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 1960, 0.68).
narrative_ontology:measurement(montevideo_constitutive_be_t1991, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 1991, 0.71).
narrative_ontology:measurement(montevideo_constitutive_be_t2008, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 2008, 0.73).
narrative_ontology:measurement(montevideo_constitutive_be_t2024, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 2024, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(montevideo_constitutive_su_t1933, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 1933, 0.55).
narrative_ontology:measurement(montevideo_constitutive_su_t1945, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 1945, 0.62).
narrative_ontology:measurement(montevideo_constitutive_su_t1960, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 1960, 0.78).
narrative_ontology:measurement(montevideo_constitutive_su_t1991, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 1991, 0.81).
narrative_ontology:measurement(montevideo_constitutive_su_t2008, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 2008, 0.79).
narrative_ontology:measurement(montevideo_constitutive_su_t2024, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__constitutive_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(montevideo_statehood_criteria__constitutive_reading, 0.12).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria__declaratory_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria__hybrid_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, un_charter_chapter_ii_membership).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, uti_possidetis_principle).

% DUAL FORMULATION NOTE:
% This constraint is one member of the montevideo_statehood_criteria constraint family. The constitutive reading (this story) treats recognition as constitutive, yielding high extraction on aspirant polities. The declaratory_reading treats recognition as acknowledgment, yielding near-zero extraction. The hybrid_reading adds normative criteria, creating a layered extraction profile. They share the same referent (statehood criteria) but instantiate different ε values and victim sets — per ε-invariance principle, they are separate constraint stories linked by affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(montevideo_statehood_criteria__constitutive_reading, institutional, 0.08).
constraint_indexing:directionality_override(montevideo_statehood_criteria__constitutive_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
