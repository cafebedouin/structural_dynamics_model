% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__expansionist_legalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__expansionist_legalist_reading, []).

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
 *   constraint_id: jihad_quranic_corpus__expansionist_legalist_reading
 *   human_readable: Expansionist Legalist Jihad Doctrine (Classical Fiqh Reading)
 *   domain: religious_law/political_theology
 *
 * SUMMARY:
 *   This constraint story models the expansionist legalist reading of jihad
 *   from the Quranic corpus as instantiated in classical Sunni fiqh (8th-14th
 *   centuries CE). The reading holds that offensive jihad is a collective
 *   obligation (fard kifaya) upon the Muslim polity to establish Islamic
 *   governance globally, subject to specific juristic conditions: formal
 *   invitation to Islam (da'wa) must precede combat; declaration is the
 *   exclusive prerogative of the legitimate imam/caliph; proportionality and
 *   non-combatant immunity rules bind conduct; conquered populations enter a
 *   regulated dhimmi status with tribute (jizya) in exchange for protection.
 *   The doctrine coordinates expansion through legal regulation but
 *   simultaneously extracts sovereignty, land, and tribute from non-Muslim
 *   populations who have no voice in the framework. The constraint is claimed
 *   by its adherents as divine law (Mountain/Rope) but structurally operates
 *   as Tangled Rope: genuine coordination of conquest logistics and spoils
 *   distribution coexists with asymmetric extraction from trapped non-Muslim
 *   populations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__expansionist_legalist_reading, 0.72).
domain_priors:suppression_score(jihad_quranic_corpus__expansionist_legalist_reading, 0.78).
domain_priors:theater_ratio(jihad_quranic_corpus__expansionist_legalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(jihad_quranic_corpus__expansionist_legalist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__expansionist_legalist_reading, tangled_rope).
narrative_ontology:human_readable(jihad_quranic_corpus__expansionist_legalist_reading, "Expansionist Legalist Jihad Doctrine (Classical Fiqh Reading)").
narrative_ontology:topic_domain(jihad_quranic_corpus__expansionist_legalist_reading, "religious_law/political_theology").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__expansionist_legalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__expansionist_legalist_reading, 'f1aff6dd-9c1e-4b9d-82d0-178b48be0132').
narrative_ontology:cs_kernel_codification('f1aff6dd-9c1e-4b9d-82d0-178b48be0132', formalized).
narrative_ontology:cs_authority_grounding('f1aff6dd-9c1e-4b9d-82d0-178b48be0132', lineage).
narrative_ontology:cs_interpretation_layer_present('f1aff6dd-9c1e-4b9d-82d0-178b48be0132').
narrative_ontology:cs_reading_relation('f1aff6dd-9c1e-4b9d-82d0-178b48be0132', jihad_quranic_corpus__defensive_spiritual_reading, coexists_with).
narrative_ontology:cs_reading_relation('f1aff6dd-9c1e-4b9d-82d0-178b48be0132', jihad_quranic_corpus__revolutionary_vanguard_reading, forecloses).
narrative_ontology:cs_axiom('f1aff6dd-9c1e-4b9d-82d0-178b48be0132', foundational, offensive_jihad_obligatory_under_conditions).
narrative_ontology:cs_axiom_status(offensive_jihad_obligatory_under_conditions, holdable).
narrative_ontology:cs_axiom_grounding('f1aff6dd-9c1e-4b9d-82d0-178b48be0132', offensive_jihad_obligatory_under_conditions, conventional).
narrative_ontology:cs_axiom('f1aff6dd-9c1e-4b9d-82d0-178b48be0132', foundational, imam_monopoly_on_declaration).
narrative_ontology:cs_axiom_status(imam_monopoly_on_declaration, holdable).
narrative_ontology:cs_axiom_grounding('f1aff6dd-9c1e-4b9d-82d0-178b48be0132', imam_monopoly_on_declaration, conventional).
narrative_ontology:cs_axiom('f1aff6dd-9c1e-4b9d-82d0-178b48be0132', secondary, invitation_before_combat).
narrative_ontology:cs_axiom_status(invitation_before_combat, holdable).
narrative_ontology:cs_axiom_grounding('f1aff6dd-9c1e-4b9d-82d0-178b48be0132', invitation_before_combat, conventional).
narrative_ontology:cs_reference_frame('f1aff6dd-9c1e-4b9d-82d0-178b48be0132', classical_fiqh_expansionist_framework).
narrative_ontology:cs_drift_state('f1aff6dd-9c1e-4b9d-82d0-178b48be0132', modern_nation_state_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('f1aff6dd-9c1e-4b9d-82d0-178b48be0132', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, caliphate_imam).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__expansionist_legalist_reading, muslim_polity).
narrative_ontology:constraint_victim(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_populations).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__expansionist_legalist_reading, divine_sovereignty_manifest_in_governance).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__expansionist_legalist_reading, sharia_as_universal_legal_order).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds monopoly on legitimate declaration of offensive jihad; sets conditions (invitation, proportionality, immunity rules); bears responsibility for just conduct and distribution of spoils; constrained by juristic consensus and risk of deposition if wars fail.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, caliphate_imam, agenda_setter,
    institutional, generational, constrained, global).

% Receives territorial expansion, resource extraction (jizya, kharaj, Fay'), and religious legitimacy from successful campaigns; individual Muslims may opt out of participation but polity-level benefits are collective; exit means migration to non-Muslim lands (hijra) which carries religious stigma.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, muslim_polity, beneficiary,
    organized, generational, mobile, global).

% Face trilemma upon Muslim army arrival: convert (join beneficiary class), accept dhimmi status with jizya and legal disabilities, or resist as combatants; no recognized sovereignty option; liminal status persists until conquest concludes; extraction is structural (land, labor, tribute) not incidental.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, non_muslim_populations, payer,
    powerless, biographical, trapped, global).

% Hold the defensive_spiritual_reading; argue offensive jihad was only for Prophet's era or defensive; excluded from this reading's legal framework because they reject the expansionist premise; their objection would be that conquest contradicts Quranic 'no compulsion in religion' (2:256).
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, defensive_spiritual_adherents, excluded,
    moderate, biographical, constrained, global).

% Hold the revolutionary_vanguard_reading; declare current rulers apostate, bypass imam authority via takfir; excluded because this reading requires legitimate imam and their premise forecloses state-mediated jihad; would object that waiting for imam permission abandons individual obligation.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, revolutionary_vanguard_adherents, excluded,
    moderate, immediate, identity_locked, global).

% Analyzes the doctrine's internal coherence, historical application, and relationship to other readings; does not collect extraction nor pay it; sees full structural asymmetry between imam monopoly and non-Muslim liminality.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__expansionist_legalist_reading, classical_fiqh_scholar, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a rule-bound legal framework for systematic expansion of Islamic governance: coordinates military mobilization, spoils distribution, treatment of conquered populations, and inter-polity relations under a single authoritative declaration, replacing ad-hoc raiding with regulated conquest.
% TRANSFER_FUNCTION: Moves political sovereignty, land (kharaj), tribute (jizya), and movable spoils (ghanima) from non-Muslim polities to the Muslim polity under caliphal authority; the imam receives 1/5 (khums) of ghanima and administers distribution; non-Muslim populations bear the transfer through conquest, subjugation, or flight.
% ABSENT_VOICES: Non-Muslim populations subject to conquest have no voice in the legal framework that determines their status; defensive_spiritual_reading adherents are excluded because they deny the expansionist premise; revolutionary_vanguard_reading adherents are excluded because they reject the imam monopoly this reading requires. All three would object if present but occupy different structural positions: the conquered are trapped, the rival readings are factional competitors.
% DISAPPEARANCE_RATIONALE: If this doctrine vanished overnight, the legal architecture legitimating offensive expansion collapses: no imam monopoly on declaration, no regulated invitation (da'wa) precondition, no dhimmi framework, no spoils distribution rules. Muslim polities would either revert to defensive-only posture (adopting defensive_spiritual_reading) or fragment into unauthorized vanguard violence (revolutionary_vanguard_reading). The conquered populations' liminal status would become legally undefined.
% FOUNDING_PROBLEM: How to legitimize and regulate expansion of Islamic rule beyond defensive war after the Prophetic era, given Quranic verses permitting fighting 'until religion is for Allah' (8:39, 2:193) and the historical reality of rapid conquests requiring legal governance.
% FOUNDING_PROBLEM_CORROBORATION: Classical jurists (al-Shaybani, al-Mawardi, Ibn Rushd) attest the obligation was live in formative period; modern reformist scholars (e.g., Fazlur Rahman, Abdullahi An-Na'im) argue the founding problem was historically contingent and the obligation lapsed with the end of the classical caliphate; no consensus exists outside the benefiting juristic tradition.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__expansionist_legalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__expansionist_legalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__expansionist_legalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jihad_quranic_corpus__expansionist_legalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__expansionist_legalist_reading, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__expansionist_legalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jihad_quranic_corpus__expansionist_legalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jihad_quranic_corpus__expansionist_legalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72) reflects systematic transfer of resources and sovereignty from conquered peoples under legal cover; the rate is calibrated by classical rules (1/5 khums to imam, 4/5 to soldiers, jizya rates, kharaj land tax) but the structural extraction is high because alternatives (independent non-Muslim polity) are suppressed. Suppression (0.78) is high because the constraint's persistence depends on military enforcement of the imam's monopoly and the dhimmi framework — non-Muslims cannot legally opt out of the trilemma. Theater ratio (0.28) is moderate-low because the legal conditions (invitation, proportionality, immunity) are genuinely operative in classical doctrine, not mere pretexts, though their observance varied in practice. Accessibility collapse (0.82) is high because once the doctrine is accepted as binding, the trilemma for non-Muslims closes almost completely. Resistance (0.55) is moderate: non-Muslim populations resisted militarily and through flight, but the legal framework itself faced internal juristic contestation (defensive reading) and external revolutionary challenge.
 *
 * PERSPECTIVAL GAP:
 *   From the caliphate_imam seat, the constraint appears as Rope: a divine coordination mechanism that regulates what would otherwise be chaotic conquest, protects non-combatants, and distributes spoils justly. From the non_muslim_populations seat, it appears as Snare: a legalized conquest machine where the 'conditions' (invitation, proportionality) are procedural fig leaves for inevitable subjugation. The engine computes this divergence from the structural asymmetry: imam monopoly on declaration + non-Muslim liminality + trapped exit = high effective extraction for payers, low for agenda-setter. The claimed_type (tangled_rope) captures the analyst's view that both perceptions are partially true — coordination and extraction are structurally fused.
 *
 * DIRECTIONALITY LOGIC:
 *   The caliphate_imam sits near the beneficiary end (d ~ 0.15): collects khums, controls declaration, gains legitimacy — but bears political risk of failed campaigns. The muslim_polity is a net beneficiary (d ~ 0.25) with mobile exit (hijra possible but stigmatized). Non_muslim_populations are full targets (d ~ 0.95): trapped, no recognized sovereignty, systematic extraction. Defensive_spiritual_adherents are constrained (d ~ 0.45): they reject the premise but operate within the same tradition. Revolutionary_vanguard_adherents are identity_locked (d ~ 0.7): their alternative reading forecloses this one's core premise (imam monopoly) but they remain trapped in the same kernel dispute. The classical_fiqh_scholar is analytical (d = 0.5 by definition).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (legitimizing post-Prophetic expansion) was live during the classical conquest era but its status is now contested: the caliphate is abolished, nation-states replaced the polity model, and international law prohibits wars of aggression. The arrangement persists in juristic texts and is invoked by revivalist movements, but the coordination function (regulating actual conquests) has atrophied while the extraction logic (legitimizing resource transfer from non-Muslims) remains potent in ideological form. This is not a Piton (theatrical maintenance of dead function) because the doctrine still generates live mobilization — but it is a Tangled Rope whose coordination target has shifted from territorial conquest to ideological recruitment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the jihad_quranic_corpus kernel, or does it collapse into the kernel itself?',
    'Comparative structural analysis of all three declared readings: if each reading produces a different beneficiary/victim structure, different exit options for non-Muslims, and different imam authority claims, they are distinct constraints with distinct ε values.',
    'If distinct, each reading gets its own constraint story with independent classification; if not distinct, the kernel is a single constraint with observer-dependent classification (violating ε-invariance).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the expansionist legalist reading is a structurally separable constraint from its sibling readings.').

omega_variable(
    divine_law_vs_juristic_construction,
    'Is the expansionist jihad obligation a genuine Mountain (divine, unchangeable) or a constructed Tangled Rope (juristic interpretation serving polity interests)?',
    'Historical analysis of whether the conditions (invitation, imam authority, proportionality) were consistently observed or selectively enforced to serve expansion; juristic debate records showing whether the obligation was treated as immutable or context-dependent.',
    'If Mountain, false_summit_mountain signature triggers (beneficiaries declared on natural-law claim); if Tangled Rope, the coordination/extraction hybrid is the honest classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_law_vs_juristic_construction, conceptual, 'Natural-law vs. constructed ambiguity for a doctrine claiming divine origin.').

omega_variable(
    extraction_contingency_on_historical_application,
    'Is the measured extraction (0.72) inherent to the doctrine''s logic, or contingent on historical imperial application by caliphates?',
    'Counterfactual: if the doctrine were applied without territorial conquest (e.g., only defensive wars), would the extraction metric collapse? Comparative study of periods when offensive jihad was suspended (e.g., post-Mongol, Ottoman defensive phase).',
    'If contingent, the high extractiveness reflects historical imperial practice, not the reading''s internal logic; if inherent, the reading''s structure necessitates extraction regardless of historical accident.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_contingency_on_historical_application, empirical, 'Whether extraction is structurally necessary to this reading or historically contingent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__expansionist_legalist_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t0, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(jiha_tr_t150, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 150, 0.18).
narrative_ontology:measurement(jiha_tr_t300, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 300, 0.22).
narrative_ontology:measurement(jiha_tr_t450, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 450, 0.25).
narrative_ontology:measurement(jiha_tr_t600, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 600, 0.28).
narrative_ontology:measurement(jiha_tr_t750, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 750, 0.3).
narrative_ontology:measurement(jiha_tr_t900, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 900, 0.32).
narrative_ontology:measurement(jiha_tr_t1050, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 1050, 0.35).
narrative_ontology:measurement(jiha_tr_t1200, jihad_quranic_corpus__expansionist_legalist_reading, theater_ratio, 1200, 0.28).

% Extraction over time
narrative_ontology:measurement(jiha_be_t0, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(jiha_be_t150, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 150, 0.68).
narrative_ontology:measurement(jiha_be_t300, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 300, 0.72).
narrative_ontology:measurement(jiha_be_t450, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 450, 0.75).
narrative_ontology:measurement(jiha_be_t600, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 600, 0.74).
narrative_ontology:measurement(jiha_be_t750, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 750, 0.73).
narrative_ontology:measurement(jiha_be_t900, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 900, 0.71).
narrative_ontology:measurement(jiha_be_t1050, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 1050, 0.68).
narrative_ontology:measurement(jiha_be_t1200, jihad_quranic_corpus__expansionist_legalist_reading, base_extractiveness, 1200, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t0, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(jiha_su_t150, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 150, 0.72).
narrative_ontology:measurement(jiha_su_t300, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 300, 0.78).
narrative_ontology:measurement(jiha_su_t450, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 450, 0.8).
narrative_ontology:measurement(jiha_su_t600, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 600, 0.79).
narrative_ontology:measurement(jiha_su_t750, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 750, 0.77).
narrative_ontology:measurement(jiha_su_t900, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 900, 0.75).
narrative_ontology:measurement(jiha_su_t1050, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 1050, 0.73).
narrative_ontology:measurement(jiha_su_t1200, jihad_quranic_corpus__expansionist_legalist_reading, suppression_requirement, 1200, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__expansionist_legalist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(jihad_quranic_corpus__expansionist_legalist_reading, 0.12).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus__defensive_spiritual_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__expansionist_legalist_reading, jihad_quranic_corpus__revolutionary_vanguard_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the jihad_quranic_corpus constraint family (three readings of one kernel). The expansionist_legalist_reading provides the classical juristic framework that the defensive_spiritual_reading restricts and the revolutionary_vanguard_reading bypasses. All three share the kernel_id but instantiate different constraints with different ε values: defensive reading has lower extractiveness (no offensive expansion), vanguard reading has higher extractiveness (no imam check, takfir expands target pool). This story links to siblings via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jihad_quranic_corpus__expansionist_legalist_reading, institutional, 0.15).
constraint_indexing:directionality_override(jihad_quranic_corpus__expansionist_legalist_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
