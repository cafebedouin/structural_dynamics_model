% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__revolutionary_vanguard_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__revolutionary_vanguard_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jihad_quranic_corpus__revolutionary_vanguard_reading
 *   human_readable: Jihad as Fard 'Ayn Against Apostate Rulers via Takfir and Emergency Jurisprudence
 *   domain: religious/political/jurisprudential
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested Quranic corpus
 *   on jihad: the revolutionary vanguard reading that frames jihad as an
 *   immediate personal obligation (fard 'ayn) for individual believers,
 *   justified by takfir doctrine (declaring rulers apostates) and emergency
 *   jurisprudence (darurah), bypassing institutional Islamic authority
 *   structures (imam, state, mainstream ulama). This reading decentralizes
 *   the authority to declare and wage jihad, eliminates the classical
 *   requirement for state authorization, overrides jurisprudential safeguards
 *   on non-combatant immunity and proportionality, and extends targeting to
 *   civilians in territory controlled by occupiers or apostate rulers. The
 *   constraint operates as a snare: it is presented as Islamic
 *   jurisprudential obligation, but functions as extraction from believers
 *   (through identity-lock and violence participation) and from civilians and
 *   scholars (through targeting and delegitimization). The measurement series
 *   shows rising extractiveness and suppression over the 40-year interval,
 *   with theater ratio remaining low and stable — the constraint's legitimacy
 *   cover is consistent but its operational extraction increases.
 *
 * KEY AGENTS:
 *   - Revolutionary vanguard actors: distributed agenda-setters who declare takfir and fard 'ayn obligation; trapped institutional actors with decentralized authority; frame the reading as liberation from corrupted state/scholarly gatekeeping.
 *   - Apostate rulers: institutional payers/victims designated as targets via takfir; their institutional authority is directly challenged as illegitimate under this reading.
 *   - Occupying military forces: regional institutional payers/victims; treated as legitimate military targets via emergency jurisprudence that eliminates state monopoly on warfare.
 *   - Civilians in conflict zones: powerless victims; become combatants under collective-guilt framework and loss of non-combatant immunity.
 *   - Mainstream Islamic scholars: institutional payers/victims; delegitimized for insisting on classical safeguards and non-vanguard authority.
 *   - Individual believers in vanguard areas: powerless payers facing fard 'ayn obligation claim, identity-locked to the reading, with exit requiring faith apostasy.
 *   - International law bodies: observer seat; document systematic violations of combatant/non-combatant distinction and proportionality.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.88).
domain_priors:suppression_score(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.91).
domain_priors:theater_ratio(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__revolutionary_vanguard_reading, snare).
narrative_ontology:human_readable(jihad_quranic_corpus__revolutionary_vanguard_reading, "Jihad as Fard 'Ayn Against Apostate Rulers via Takfir and Emergency Jurisprudence").
narrative_ontology:topic_domain(jihad_quranic_corpus__revolutionary_vanguard_reading, "religious/political/jurisprudential").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__revolutionary_vanguard_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__revolutionary_vanguard_reading, '01de6ea6-a5a7-4261-95e4-910ff93b01e6').
narrative_ontology:cs_kernel_codification('01de6ea6-a5a7-4261-95e4-910ff93b01e6', fixed_text).
narrative_ontology:cs_authority_grounding('01de6ea6-a5a7-4261-95e4-910ff93b01e6', extraction).
narrative_ontology:cs_reading_relation('01de6ea6-a5a7-4261-95e4-910ff93b01e6', jihad_quranic_corpus__defensive_spiritual_reading, coexists_with).
narrative_ontology:cs_reading_relation('01de6ea6-a5a7-4261-95e4-910ff93b01e6', jihad_quranic_corpus__expansionist_legalist_reading, coexists_with).
narrative_ontology:cs_axiom('01de6ea6-a5a7-4261-95e4-910ff93b01e6', foundational, takfir_individual_authority).
narrative_ontology:cs_axiom_status(takfir_individual_authority, holdable).
narrative_ontology:cs_axiom_grounding('01de6ea6-a5a7-4261-95e4-910ff93b01e6', takfir_individual_authority, deontological).
narrative_ontology:cs_axiom('01de6ea6-a5a7-4261-95e4-910ff93b01e6', foundational, emergency_doctrine_overrides_safeguards).
narrative_ontology:cs_axiom_status(emergency_doctrine_overrides_safeguards, holdable).
narrative_ontology:cs_axiom_grounding('01de6ea6-a5a7-4261-95e4-910ff93b01e6', emergency_doctrine_overrides_safeguards, empirically_contingent).
narrative_ontology:cs_reference_frame('01de6ea6-a5a7-4261-95e4-910ff93b01e6', classical_imam_authority_jurisprudential_gate).
narrative_ontology:cs_drift_state('01de6ea6-a5a7-4261-95e4-910ff93b01e6', contemporary_vanguard_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('01de6ea6-a5a7-4261-95e4-910ff93b01e6', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, apostate_rulers).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, occupying_powers).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, civilians_in_conflict_zones).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, mainstream_islamic_scholars).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__revolutionary_vanguard_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(jihad_quranic_corpus__revolutionary_vanguard_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__revolutionary_vanguard_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jihad_quranic_corpus__revolutionary_vanguard_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very high (0.88 at interval end) because the constraint generates multiple overlapping extractions: (1) violence participation extracted from individual believers through identity-fused obligation; (2) targeting authority extracted from institutional gatekeepers to decentralized actors; (3) non-combatant immunity eliminated, making civilians targetable; (4) institutional legitimacy extracted from rulers and scholars through takfir and delegitimization. The series shows extraction rising from 0.72 to 0.88 over 40 years as vanguard movements mature and their ability to enforce the reading on larger populations increases. Suppression is even higher (0.91) because the constraint's persistence depends on: (1) identity-locking believers to the reading through fusion with Islamic faith; (2) eliminating alternative institutional paths (imam authority, state monopoly, mainstream scholarly consensus); (3) suppressing dissent within vanguard areas through takfir-threat and violence; (4) preventing exit by making apostasy the only alternative to participation. Theater ratio is low (0.22) and stable because while legitimacy framing is constant (Islamic jurisprudence, emergency doctrine, fard 'ayn obligation), the actual operational function — extraction and violence — remains consistent with the framing; there is little performative drift because the vanguard is not defending an atrophied coordination function but actively prosecuting a decentralized violence campaign justified by emergency. The measurement grid shares one time axis across all three metrics at every examined point (0, 5, 10, 15, 20, 25, 30, 40), ensuring that statements like 'suppression rose while extractiveness stabilized' are not artifacts of misaligned grids.
 *
 * PERSPECTIVAL GAP:
 *   The vanguard actor seat and the victim seats (civilians, scholars, rulers, believers) compute dramatically different constraint types from the same structural data. From the vanguard's position, the reading is genuine coordination: it is distributing Islamic obligation justly to all believers and overriding corrupted institutional gatekeeping — extractiveness appears low from this seat because the extraction is reframed as righteous obligation. From the victim seats, the same structure operates as pure snare: the obligation is imposed without consent, the emergency framing bypasses safeguards that protect civilians, and exit requires apostasy. The engine computes per-seat classifications from the structural data (power, exit_options, beneficiary/victim position); the vanguard seat's d-value trends toward 0.4–0.5 (symmetric or moderate extraction framed as coordination), while civilian and believer seats compute d near 0.85–0.95 (trapped targets). This perspectival divergence is the measurement the framework exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Revolutionary vanguard actors are structural beneficiaries in a limited sense: they gain authority and agency in acting as agenda-setters, but they are also trapped (exit_options=trapped) because the reading fuses their own identity to the obligation. More precisely, they benefit from authority distribution, but they are not unambiguous beneficiaries collecting rents; they are true believers enforcing a reading they have internalized. Apostate rulers are full targets (d=1.0): they bear the cost of being declared apostate, lose institutional legitimacy, and face decentralized targeting. Occupying forces are targets (d=0.95+): they become legitimate targets under emergency jurisprudence. Civilians are full targets (d=0.95+): they lose non-combatant immunity, become targetable via collective guilt, and are trapped. Mainstream scholars are targets (d=0.80+): they are delegitimized for maintaining safeguards. Individual believers are complex: they are nominally beneficiaries (the reading claims to liberate them from corrupted authority) but are actually targets (d=0.85+) because the liberation is purchased through identity-lock and violence participation with exit blocked by faith cost. No directionality overrides are needed because the structural derivation from beneficiary/victim + exit + power produces the correct d values: the apparent benefit to vanguard actors (distributed authority) is overwhelmed by their trapped exit, and the apparent benefit to believers (liberation) is overwhelmed by their identity-lock.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (injustice perpetrated by apostate rulers and occupiers) is real and live; the vanguard reading claims to solve it through immediate, decentralized obligation. The constraint persists and even intensifies (extractiveness rising 0.72→0.88) despite classical Islamic jurisprudence's counter-claim that the safeguards it maintains (imam authority, proportionality, non-combatant immunity) are the actual solution. The classification prevents mislabeling this as coordination or rope: the reading IS presented as coordination (distributing jihad obligation fairly, overriding corrupted gatekeeping), but its operation is pure snare (extraction from believers through identity-lock, extraction of non-combatant immunity through emergency override, extraction of institutional authority through decentralization). Mandatrophy is not fully present (the founding problem is genuinely addressed in the vanguard's logic), but there is substantial asymmetry: mainstream Islam claims the same founding problem is addressed more justly through classical safeguards, and victim testimony confirms that the vanguard reading produces extraction without solving the underlying injustice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_vs_misuse_distinction,
    'Does this constraint describe a valid Islamic jurisprudential reading that some mainstream scholars hold (even if minority), or does it describe a misapplication/distortion of Islamic sources that all major scholarly traditions explicitly reject?',
    'Textual analysis by credentialed Islamic scholars from multiple traditions (Sunni, Shi''a, etc.) examining whether the reading''s hermeneutical moves (takfir authority, emergency override of safeguards, collective guilt) can be grounded in classical jurisprudential sources or represent a break with them.',
    'If the reading is a valid minority reading, the constraint''s classification remains snare (an extractive interpretation with victims). If it is a misapplication with no scholarly grounding, the constraint becomes a false-summit candidate: presented as Islamic jurisprudence but operating as pure ideology/extraction without the hermeneutical warrant claimed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_misuse_distinction, conceptual, 'Whether the reading is a legitimate Islamic jurisprudential position or a break from Islamic tradition presented as legitimacy.').

omega_variable(
    emergency_doctrine_scope,
    'Under Islamic jurisprudence, what conditions justify emergency doctrine (darurah) overriding normal safeguards? Does this reading correctly invoke darurah, or does it use emergency language to bypass safeguards that Islamic law holds even in genuine emergency?',
    'Comparative study of how classical and modern Islamic jurisprudence defines and constrains emergency doctrine; examination of whether the vanguard reading''s invocation of darurah meets the scholarly consensus criteria (imminent threat, no alternative, proportional response, intent to restore normal law afterward) or uses the language while violating the criteria.',
    'If emergency is correctly invoked, the constraint represents a contested but internally coherent reading. If emergency language is used while violating darurah criteria, the constraint is a snare that uses legitimacy cover to justify extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_doctrine_scope, empirical, 'Whether the constraint correctly invokes Islamic emergency doctrine or misapplies it.').

omega_variable(
    decentralization_vs_mob_rule,
    'Does eliminating state/imam authority genuinely distribute legitimate authority, or does it collapse legitimate authority entirely, leaving only individual judgment unmoored from institutional constraint?',
    'Historical analysis of how decentralized vanguard movements actually make targeting decisions: are decisions made with jurisprudential reasoning and internal accountability, or based on individuals'' unconstrained assertion of who deserves takfir and violence? Do movements develop alternative institutional gatekeeping (parallel councils, scholars) that replicate the classical function, or do they abandon gatekeeping altogether?',
    'If decentralization preserves some institutional reasoning and accountability, the constraint remains extractive but retains some internal coherence. If decentralization = absence of constraint, the reading operates as pure extraction justified by emergency rhetoric with no actual gatekeeping — a deeper snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(decentralization_vs_mob_rule, empirical, 'Whether decentralization distributes authority or abolishes it.').

omega_variable(
    identity_locked_exit_mechanism,
    'For believers taught this reading, what is the actual exit cost? Can they reject the fard ''ayn obligation without rejecting Islam, or does the reading fuse the obligation to Islamic identity such that exit requires faith apostasy?',
    'Qualitative interviews with vanguard members and defectors about what intellectual and social costs they faced when questioning the reading; examination of whether mainstream Islamic teaching provides a clear alternative interpretation of fard ''ayn that individual believers can adopt while remaining in good standing in their communities.',
    'If exit requires faith apostasy, suppression is even more structurally embedded than the metric suggests. If exit is possible within Islam, the identity-lock is partial and the suppression metric should account for available alternative interpretations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_exit_mechanism, empirical, 'Whether identity lock to this reading requires apostasy or allows internal dissent.').

omega_variable(
    quranic_corpus_reading_contest,
    'Is this reading one defensible interpretation of the Quranic corpus on jihad, or is it one interpretation among readings with genuinely equal hermeneutical standing in classical Islam?',
    'Textual analysis by specialists in Islamic jurisprudence (usul al-fiqh) examining the hermeneutical authority of sources cited by each reading (this one, defensive_spiritual_reading, expansionist_legalist_reading) and the classical scholarly consensus on how the Quranic corpus should be reconciled.',
    'Different hermeneutical standing between readings would affect the authority frame — this reading might be minority but defensible (still a snare, but with internal legitimacy claim), or it might be a clear departure from classical consensus that borrows authority language while operating outside the tradition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(quranic_corpus_reading_contest, conceptual, 'The canonical reading standing of this interpretation versus siblings in classical Islamic tradition.').

omega_variable(
    suppression_internalization_mechanism,
    'Is the suppression of dissent structural (physical coercion, excommunication threat, exit barriers) or internalized (believers have absorbed the reading as faith truth and self-suppress dissent)?',
    'Analysis of how the reading is transmitted and maintained in vanguard communities: does dissent face physical retaliation or social excommunication (structural), or do believers self-suppress doubts because they have internalized the reading as Islamic truth (internalized)? Historical trajectories of members who have exited: do they report structural barriers that disappeared once they physically left, or do they carry the suppression with them even after exit?',
    'If structural, removing the enforcement machinery would allow dissent to surface. If internalized, the suppression persists even after physical exit — the constraint has been incorporated into believers'' identity and epistemic frameworks. This affects the actual cost of exit and the sustainability of the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_mechanism, empirical, 'Whether suppression of dissent operates through coercion or internalization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__revolutionary_vanguard_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(jiha_tr_t5, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 5, 0.11).
narrative_ontology:measurement(jiha_tr_t10, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(jiha_tr_t15, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 15, 0.17).
narrative_ontology:measurement(jiha_tr_t20, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(jiha_tr_t25, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 25, 0.21).
narrative_ontology:measurement(jiha_tr_t30, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(jiha_tr_t40, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(jiha_be_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(jiha_be_t5, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 5, 0.76).
narrative_ontology:measurement(jiha_be_t10, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 10, 0.81).
narrative_ontology:measurement(jiha_be_t15, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 15, 0.84).
narrative_ontology:measurement(jiha_be_t20, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 20, 0.86).
narrative_ontology:measurement(jiha_be_t25, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 25, 0.87).
narrative_ontology:measurement(jiha_be_t30, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 30, 0.88).
narrative_ontology:measurement(jiha_be_t40, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 40, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement(jiha_su_t5, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 5, 0.81).
narrative_ontology:measurement(jiha_su_t10, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 10, 0.85).
narrative_ontology:measurement(jiha_su_t15, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 15, 0.87).
narrative_ontology:measurement(jiha_su_t20, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 20, 0.89).
narrative_ontology:measurement(jiha_su_t25, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 25, 0.9).
narrative_ontology:measurement(jiha_su_t30, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 30, 0.91).
narrative_ontology:measurement(jiha_su_t40, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 40, 0.91).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__revolutionary_vanguard_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.12).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus__defensive_spiritual_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus__expansionist_legalist_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, islamic_state_authority_monopoly_on_legitimate_force).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, classical_islamic_jurisprudential_gatekeeping).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel JIHAD_QURANIC_CORPUS. Sibling constraints instantiate the DEFENSIVE_SPIRITUAL_READING (Mountain-leaning, internal struggle + defensive response with safeguards) and EXPANSIONIST_LEGALIST_READING (Tangled Rope, institutional conditions maintained). The three readings coexist in Islamic jurisprudence but differ sharply in whether state/imam authority is required (expansionist and defensive readings maintain institutional gatekeeping; this reading bypasses it) and whether non-combatant immunity is preserved (the defensive and expansionist readings maintain it; this reading overrides it via emergency doctrine). The ε values differ substantially: defensive-reading and expansionist-reading have lower extractiveness (higher coordination function, maintained safeguards); this reading has very high extractiveness (extraction from believers, elimination of safeguards). The network links show how adoption of this reading affects adjacent constraint systems: it directly challenges state monopoly on legitimate force (affects_constraints includes that institutional constraint) and operates by delegitimizing classical jurisprudential gatekeeping (affects both the institutional gatekeeping structure itself and the defensive/expansionist readings that depend on it for authority).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
