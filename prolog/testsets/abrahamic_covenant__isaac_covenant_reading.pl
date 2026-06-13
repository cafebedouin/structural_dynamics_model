% ============================================================================
% CONSTRAINT STORY: abrahamic_covenant__isaac_covenant_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abrahamic_covenant__isaac_covenant_reading, []).

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
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: abrahamic_covenant__isaac_covenant_reading
 *   human_readable: Abrahamic Covenant Transmitted Exclusively Through Isaac (Institutional Jewish Reading)
 *   domain: religious_studies/institutional_authority/theological_interpretation
 *
 * SUMMARY:
 *   Genesis 17:19-21 records God's explicit statement to Abraham: 'My
 *   covenant I will establish with Isaac.' Rabbinic tradition interprets this
 *   passage as mandating exclusive transmission of Abraham's covenant through
 *   Isaac's line, explicitly excluding Ishmael from covenantal inheritance.
 *   This reading has been institutionalized across 2000 years of Jewish
 *   theology, law, and communal practice. Islamic tradition contests this
 *   reading, holding instead that the covenant continues through Ishmael to
 *   Muhammad and the Islamic community. The Isaac-exclusive reading operates
 *   as a constraint because its maintenance requires active institutional
 *   enforcement: suppression of alternative readings, exclusion of Islamic
 *   covenant claims from legitimacy, and reinforcement through education,
 *   liturgy, and theological discourse. The constraint benefits Jewish
 *   institutional continuity (by establishing a clear, exclusive narrative of
 *   covenant inheritance) while extracting costs from Ishmael-lineage
 *   claimants and Islamic tradition (by delegitimizing their covenant claims
 *   and forcing them to maintain competing readings against institutional
 *   pressure). This is a TANGLED ROPE constraint: it solves a genuine
 *   coordination problem (Jewish identity and covenant continuity across
 *   diaspora and historical disruption) AND operates as asymmetric extraction
 *   (benefiting rabbinic authority and Jewish institutional continuity while
 *   imposing costs on excluded parties). Enforcement is active: rabbinic
 *   authority maintains the reading through exclusion of alternatives,
 *   delegitimation of competing exegesis, and institutional transmission
 *   mechanisms. The claim/metric independence rule requires that the
 *   constraint be CLAIMED as the institutional reading asserts it
 *   (coordination function, theological doctrine) while the metrics describe
 *   the extraction and enforcement actually observed. The engine computes
 *   whether the measured extraction and suppression exceed what coordination
 *   function requires.
 *
 * KEY AGENTS:
 *   - rabbinic_authority_tradition: Institutional defender and enforcer of the Isaac-exclusive reading; uses institutional power to maintain the reading's authority and suppress alternatives.
 *   - jewish_institutional_continuity: Beneficiary that depends structurally on the reading for narrative coherence of Jewish peoplehood and covenant inheritance.
 *   - jewish_faithful_practitioners: Practitioners who receive the reading as the framework of their religious identity; benefit from coherent theological narrative; pay through identity-lock that prevents exit without losing religious identity.
 *   - ishmael_lineage_claimants: Victim set (including Islamic tradition) that is structurally excluded from covenant inheritance by the reading; faces institutional pressure from rabbinic and Christian authorities enforcing the exclusive reading.
 *   - islamic_tradition: Organized victim that maintains competing covenant reading (Ishmael as covenant bearer) against institutional pressure from Jewish and Christian institutional authorities.
 *   - christian_supersessionist_tradition: Observer seat (primarily beneficiary as secondary role) that, despite fundamental disagreement with rabbinic Judaism, reinforces the exclusion of Ishmael and thus shares structural interest with rabbinic authority in suppressing Islamic covenant claims.
 *   - modern_critical_biblical_scholarship: Observer seat that documents the Isaac-exclusive reading as ONE reading among textual alternatives; scholars face institutional pressure from religious authorities defending the reading's authority.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abrahamic_covenant__isaac_covenant_reading, 0.81).
domain_priors:suppression_score(abrahamic_covenant__isaac_covenant_reading, 0.78).
domain_priors:theater_ratio(abrahamic_covenant__isaac_covenant_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(abrahamic_covenant__isaac_covenant_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abrahamic_covenant__isaac_covenant_reading, tangled_rope).
narrative_ontology:human_readable(abrahamic_covenant__isaac_covenant_reading, "Abrahamic Covenant Transmitted Exclusively Through Isaac (Institutional Jewish Reading)").
narrative_ontology:topic_domain(abrahamic_covenant__isaac_covenant_reading, "religious_studies/institutional_authority/theological_interpretation").

domain_priors:requires_active_enforcement(abrahamic_covenant__isaac_covenant_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(abrahamic_covenant__isaac_covenant_reading, '715317f5-4458-4259-a8ee-1899475206e6').
narrative_ontology:cs_kernel_codification('715317f5-4458-4259-a8ee-1899475206e6', fixed_text).
narrative_ontology:cs_authority_grounding('715317f5-4458-4259-a8ee-1899475206e6', lineage).
narrative_ontology:cs_interpretation_layer_present('715317f5-4458-4259-a8ee-1899475206e6').
narrative_ontology:cs_reading_relation('715317f5-4458-4259-a8ee-1899475206e6', abrahamic_covenant__ishmael_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('715317f5-4458-4259-a8ee-1899475206e6', abrahamic_covenant__land_promise_constraint, influences).
narrative_ontology:cs_reading_relation('715317f5-4458-4259-a8ee-1899475206e6', abrahamic_covenant__christian_supersessionist_reading, coexists_with).
narrative_ontology:cs_axiom('715317f5-4458-4259-a8ee-1899475206e6', foundational, isaac_exclusive_covenant_inheritance).
narrative_ontology:cs_axiom_status(isaac_exclusive_covenant_inheritance, holdable).
narrative_ontology:cs_axiom_grounding('715317f5-4458-4259-a8ee-1899475206e6', isaac_exclusive_covenant_inheritance, theological).
narrative_ontology:cs_axiom('715317f5-4458-4259-a8ee-1899475206e6', foundational, ishmael_explicitly_excluded_from_covenant).
narrative_ontology:cs_axiom_status(ishmael_explicitly_excluded_from_covenant, holdable).
narrative_ontology:cs_axiom_grounding('715317f5-4458-4259-a8ee-1899475206e6', ishmael_explicitly_excluded_from_covenant, theological).
narrative_ontology:cs_reference_frame('715317f5-4458-4259-a8ee-1899475206e6', isaac_exclusive_covenantal_lineage).
narrative_ontology:cs_drift_state('715317f5-4458-4259-a8ee-1899475206e6', contemporary_interfaith_contestation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('715317f5-4458-4259-a8ee-1899475206e6', '').
narrative_ontology:cs_kernel_id(abrahamic_covenant__isaac_covenant_reading, abrahamic_covenant).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, jewish_institutional_continuity).
narrative_ontology:constraint_beneficiary(abrahamic_covenant__isaac_covenant_reading, rabbinic_authority_tradition).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, ishmael_lineage_claimants).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, islamic_tradition_covenant_claims).
narrative_ontology:constraint_victim(abrahamic_covenant__isaac_covenant_reading, pre_rabbinic_alternative_readings).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(abrahamic_covenant__isaac_covenant_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(abrahamic_covenant__isaac_covenant_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abrahamic_covenant__isaac_covenant_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(abrahamic_covenant__isaac_covenant_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(abrahamic_covenant__isaac_covenant_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.81 at interval end) is high because the reading reserves covenant status exclusively for Jewish institutional continuity, transferring all covenantal capital FROM Ishmael's line TO Isaac's line. This is not a minor interpretive difference; it defines who is inside and outside the covenant community. The extraction is sustained not by accident but by deliberate institutional enforcement across 2000 years (measurement series shows extraction rising from 0.55 at t0 to 0.81 at t2000, demonstrating accumulation). Suppression (0.78) is high because the reading's persistence depends on actively excluding alternative readings: Islamic covenant claims must be delegitimized; pre-rabbinic Jewish readings must be overwritten; modern critical scholarship must be treated as exegetically false. Accessibility to alternatives collapses from 0.50 at t0 to 0.78 at t2000 at the individual level (faithful practitioners internalize the reading as non-negotiable), while organizational and institutional levels show even steeper collapse (0.85+). This high accessibility collapse indicates the constraint functions like a natural law for practitioners embedded in the tradition (the reading appears inevitable, not chosen). Theater ratio (0.42) reflects that the reading carries genuine theological content (not pure theater), but a growing share of institutional effort defends covenant exclusivity rather than theological truth — institutional preservation increasingly dominates the reading's maintenance over time. Resistance (0.71) is substantial, indicating that Islamic, Christian, and academic counter-readings persist despite institutional suppression. The measurement series show monotonic accumulation in extractiveness and suppression, with theater ratio stabilizing after t1500 (suggesting the institutional machinery reached steady-state enforcement).
 *
 * PERSPECTIVAL GAP:
 *   The rabbinic authority and jewish_institutional_continuity seats experience the constraint as coordination function (solving the problem of covenant identity in diaspora) and theological truth (correctly interpreting Genesis 17). From these seats, enforcement is experienced as transmission of truth and maintenance of authentic tradition, not suppression. The ishmael_lineage_claimants and islamic_tradition seats experience the constraint as asymmetric extraction: the same institutional enforcement appears as suppression of their legitimate covenant reading and delegitimization of their religious tradition. Jewish faithful practitioners sit between: they receive genuine coordination benefit (coherent theological narrative, community membership) and simultaneously bear extraction cost (the identity-lock that prevents them from accepting Islamic covenant claims without leaving Judaism). Modern critical biblical scholars, occupying an observer seat, see the constraint's operation as institutional power enforcing one reading among textual alternatives. Christian observers see institutional reinforcement of a reading that happens to support Christian supersessionism (the exclusion of Ishmael reinforces Christian claims that post-Abrahamic revelation transferred to Christianity). The engine computes per-seat type classification from these divergent perspectives; the wide divergence is the structural fact the constraint story exists to model.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation follows from beneficiary/victim declarations and exit options. Rabbinic authority and jewish_institutional_continuity are beneficiaries (d near 0.0 — they collect covenant-inheritance capital); their identity-lock and institutional power mean they cannot exit without dissolving institutional continuity (exit_options: trapped for rabbinic authority; identity_locked for jewish_institutional_continuity even though they are beneficiaries because exiting would mean losing the institutional continuity they benefit from). Ishmael_lineage_claimants and islamic_tradition are victims (d near 1.0 — they bear exclusion cost); their identity-lock prevents exit without abandoning Ishmael's covenant role in Islamic theology. Jewish faithful practitioners occupy hybrid position: beneficiary in receiving coordination function; payer in bearing identity-lock (cannot accept Islamic reading without leaving Judaism). The coercion_grid shows suppression rising across all four levels (structural, organizational, class, individual), with individual-level suppression reaching 0.65 at t2000 — practitioners internalize the reading as non-negotiable, suggesting partial identity-lock mechanism. The directionality for each seat is derivable from power, exit_options, and beneficiary/victim membership; no override is needed because the structural data capture the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (covenantal identity continuity in diaspora) was live at t0 and remains contested at t2000. Rabbinic and Jewish institutional authorities affirm it as live (the need to maintain covenant continuity remains relevant). Islamic tradition contests it: Islamic theology holds the covenant passes through Ishmael to Muhammad, so there is no 'Jewish continuity problem' — only an institutional reading that excludes Ishmael to ground Jewish particularity. Modern scholarship contests it: historians argue the founding problem was solved in antiquity; the reading persists now as tradition and institutional practice rather than response to a live problem. The disappearance_verdict is 'contested' (the parties dispute what would rearrange if the reading vanished). The mismatch between founding_problem_status (contested) and disappearance_verdict (contested) prevents mandatrophy classification: the constraint is neither clearly resolved nor clearly abandoned. However, the measurement series show accumulating extraction (0.55→0.81 over 2000 years) despite contested founding status, suggesting the constraint's persistence increasingly depends on institutional benefit (extraction) rather than founding-problem resolution. This pattern indicates POTENTIAL mandatrophy: the founding problem has shifted from live (t0) to contested/dead (t2000), while extraction has accumulated, suggesting institutional benefit increasingly dominates the reading's maintenance. The T17 abductive trigger (mountain_extraction_accumulation) would flag this as a candidate for mandatrophy review if the constraint were classified as mountain (which it is not — it's tangled_rope, so T17 does not fire). However, the measurement data establish the empirical fact of accumulation that mandatrophy investigation would examine.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_contest_identity,
    'Is the Isaac-exclusive reading a constraint, or is the contested kernel itself the constraint?',
    'The committer frame (OQ-83 Rules 1–2) authorizes generating THIS reading as ONE constraint among sibling readings. The kernel contest is routed to this omega: it names the structural uncertainty whether the object of analysis is the reading (a specific institutional interpretation with determinate ε) or the kernel (the contested textual tradition that admits multiple readings). This story takes the reading as the constraint; sibling stories take other readings. Each reading is a separate constraint with separate ε values, not facets of one polymorphic constraint.',
    'If the constraint object were the kernel rather than the reading, ε would be indeterminate (the kernel permits multiple readings); structural data (beneficiary, victim, enforcement mechanism) would differ per reading. By assigning the constraint to the reading, ε is determinate (0.81 for this reading''s extraction); structural data are specific to this reading''s operation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_identity, conceptual, 'Kernel vs. reading as the constraint object.').

omega_variable(
    theological_truth_vs_institutional_extraction,
    'Is the Isaac-exclusive reading true as a theological proposition, or is it extracted benefit for rabbinic institutional power?',
    'The framework brackets theological truth (whether the reading is doctrinally correct). It measures whether the reading''s persistence depends on institutional enforcement and whether identifiable parties benefit from its maintenance. Both conditions hold. The ambiguity is irreducible: external observers cannot adjudicate whether the reading''s truth-value explains its enforcement, or whether institutional benefit explains its enforcement and retrospective theological rationalization provides the cover. Theologically committed parties affirm the first; institutional-analysis observers affirm the second.',
    'If the reading''s persistence is explained by theological truth-claim alone, suppression and enforcement should be minimal (the truth needs no suppression). High measured suppression (0.78) suggests institutional benefit as a primary driver; this narrows the set of plausible explanations but does not eliminate theological truth-claim as a genuine component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theological_truth_vs_institutional_extraction, preference, 'Whether the constraint operates as theological doctrine or institutional extraction or both.').

omega_variable(
    ishmael_lineage_identity_lock,
    'Is the measured identity-lock for Ishmael lineage claimants structural (economic, legal, geographic barriers to exit) or internalized (belief, identity fusion, cognitive patterns that persist after structural barriers are removed)?',
    'Post-suppression trajectory: in contexts where Islamic communities have institutional power and can enforce counter-readings without suppression, do the suppression effects on Jewish participants persist? If yes, identity-lock is partially internalized. Conversely, where Jewish institutional authority loses suppressive power, do Islamic communities freely maintain their reading? If no, structural dependence persists despite reduced suppression.',
    'If identity-lock is purely structural, removing rabbinic institutional enforcement would enable free exit (Ishmael-lineage claimants could adopt the Isaac-exclusive reading without cognitive cost). If partially internalized, the suppression cost persists in individual belief and identity-formation even after institutional removal. High internalized component makes the constraint''s effective suppression higher than the structural measure (0.78) suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ishmael_lineage_identity_lock, empirical, 'Whether identity-lock in religious covenant reading is structural or internalized suppression.').

omega_variable(
    kernel_reading_identity,
    'Which sibling reading of the abrahamic_covenant kernel is THIS constraint instantiating?',
    'This constraint instantiates the isaac_covenant_reading: the interpretation that Genesis 17:19-21 mandates exclusive transmission of covenant through Isaac, explicitly excluding Ishmael. This is the committer-axis declaration. The sibling readings (ishmael_covenant_reading, land_promise_constraint, and implicitly christian_supersessionist_reading) are OTHER constraints, generated separately. The ε-invariance principle requires separate files for readings with different ε values, different beneficiary/victim structures, and different enforcement mechanisms.',
    'Each reading is a distinct constraint with its own classification. The Isaac-exclusive reading shows high extraction (0.81) because exclusion of Ishmael''s line transfers covenant status exclusively to Jewish institutional continuity. The ishmael_covenant_reading, if generated, would show different ε (likely higher or lower depending on whether Islamic institutional enforcement is measured) and different victim/beneficiary sets. The readings do NOT collapse into one polymorphic constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is the isaac_covenant_reading of the abrahamic_covenant kernel.').

omega_variable(
    rabbinic_institutional_trap,
    'Is the rabbinic institutional commitment to the Isaac-exclusive reading itself trapped (cannot exit without institutional dissolution) or can rabbinic authority reform or abandon the reading without losing institutional continuity?',
    'Historical: have rabbinic communities successfully revised earlier authoritative interpretations without institutional dissolution? If yes, exit is not fully trapped. Contemporary: do rabbinic reformers or critics who challenge the Isaac-exclusive reading face institutional penalties (ostracism, delegitimation, loss of authority status)? If yes, structural trapping is enforced.',
    'If rabbinic authority is fully trapped by the reading, the reading''s persistence is structural lock-in (institutional continuity depends on reading maintenance). If rabbinic authority retains reform capacity, the reading persists because of institutional benefit (extraction) rather than structural necessity. Measured suppression (0.78) suggests some institutional flexibility exists (not absolute lock) but at substantial cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rabbinic_institutional_trap, empirical, 'Whether rabbinic institutional authority is structurally trapped by the Isaac-exclusive reading or retains reform capacity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abrahamic_covenant__isaac_covenant_reading, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abra_tr_t0, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(abra_tr_t250, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 250, 0.28).
narrative_ontology:measurement(abra_tr_t500, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 500, 0.33).
narrative_ontology:measurement(abra_tr_t1000, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 1000, 0.4).
narrative_ontology:measurement(abra_tr_t1500, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 1500, 0.42).
narrative_ontology:measurement(abra_tr_t2000, abrahamic_covenant__isaac_covenant_reading, theater_ratio, 2000, 0.42).

% Extraction over time
narrative_ontology:measurement(abra_be_t0, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(abra_be_t250, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 250, 0.68).
narrative_ontology:measurement(abra_be_t500, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 500, 0.74).
narrative_ontology:measurement(abra_be_t1000, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 1000, 0.79).
narrative_ontology:measurement(abra_be_t1500, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 1500, 0.81).
narrative_ontology:measurement(abra_be_t2000, abrahamic_covenant__isaac_covenant_reading, base_extractiveness, 2000, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(abra_su_t0, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(abra_su_t250, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 250, 0.58).
narrative_ontology:measurement(abra_su_t500, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 500, 0.68).
narrative_ontology:measurement(abra_su_t1000, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 1000, 0.74).
narrative_ontology:measurement(abra_su_t1500, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 1500, 0.77).
narrative_ontology:measurement(abra_su_t2000, abrahamic_covenant__isaac_covenant_reading, suppression_requirement, 2000, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abrahamic_covenant__isaac_covenant_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(abrahamic_covenant__isaac_covenant_reading, 0.12).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, abrahamic_covenant__ishmael_covenant_reading).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, abrahamic_covenant__land_promise_constraint).
narrative_ontology:affects_constraint(abrahamic_covenant__isaac_covenant_reading, abrahamic_covenant__christian_supersessionist_reading).

% DUAL FORMULATION NOTE:
% The abrahamic_covenant kernel decomposes into four constraint stories: (1) isaac_covenant_reading (this story) — exclusive transmission through Isaac to Jewish institutional continuity; (2) ishmael_covenant_reading — inclusive transmission through Ishmael to Islamic tradition; (3) land_promise_constraint — territorial covenant component; (4) christian_supersessionist_reading (implicit) — covenant transferred to Christian Church. Each reading is a separate constraint with distinct ε, beneficiary/victim structure, and enforcement mechanism. ε-invariance principle: measuring the constraint via Isaac-exclusive reading yields high extraction (0.81); measuring via Ishmael-inclusive reading yields different extraction profile; measuring via land promise yields territorial-dispute extraction. These are not observational variations of one constraint — they are structurally distinct constraints arising from different readings of the kernel. All four stories share kernel_id: abrahamic_covenant; each story's constraint_id carries the reading_id (isaac_covenant_reading, ishmael_covenant_reading, etc.).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
