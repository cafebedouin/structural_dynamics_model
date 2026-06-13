% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__study_as_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__study_as_exercise, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: temple_sacrifice_commitment__study_as_exercise
 *   human_readable: Temple Sacrifice Commitment — Study as Exercise Reading
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   The temple's destruction (70 CE) severed the material conditions for
 *   sacrifice — the commitment remained binding, but its instantiation became
 *   impossible. The studying community of Jewish tradition adopted a reading
 *   of the divine command that reframes study itself as the performance of
 *   the obligation. This is not archival preservation of a defunct practice;
 *   rather, intellectual engagement with halakhic texts about sacrifice
 *   constitutes the actual exercise of the covenant. The claim is grounded in
 *   a verse applied hermeneutically: 'One who occupies himself with study of
 *   the offering is regarded as though he offered it' (Talmud, Menachot
 *   110a). Within this reading, the studying community occupies the full
 *   commitment through textual and oral engagement. This constraint story
 *   instantiates ONLY this reading — not the performance_only reading (which
 *   contests that study counts as performance), not the hybrid_preparatory
 *   reading (which holds study is suspended preparedness), not the
 *   symbolic_transformation reading (which claims authorized change). These
 *   are separate constraints with different ε values and different authority
 *   grounds. This story models the study_as_exercise reading as a natural law
 *   emergent from the commitment system's own internal logic: once the
 *   material conditions are removed, the commitment's persistence logically
 *   implies that some form of non-material instantiation must count as valid
 *   performance — otherwise the command would be impossible and thus not
 *   binding. From this reading's standpoint, the structure is mathematically
 *   inevitable given the axioms.
 *
 * KEY AGENTS:
 *   - Studying community: the collective of learners maintaining the halakhic tradition through continuous engagement with sacrifice texts. Within this reading, they are the sole beneficiary — covenant fidelity accrues to them, no extraction occurs.
 *   - Textual tradition (Mishnah, Talmud, codes): the binding kernel of the commitment, transmissible only through study. Authority grounds itself in lineage — these texts are the received law from Sinai via rabbinical chain of transmission.
 *   - Divine command: the foundational axiom — the commitment to study IS the commitment to perform, according to this reading's hermeneutic.
 *   - Performance-only reading: an alternative voice that would contest this reading's adequacy; present within the tradition but subordinate in institutional weight.
 *   - Hybrid-preparatory and symbolic-transformation readings: additional alternatives; excluded from this story but present in the kernel contest.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__study_as_exercise, 0.0).
domain_priors:suppression_score(temple_sacrifice_commitment__study_as_exercise, 0.0).
domain_priors:theater_ratio(temple_sacrifice_commitment__study_as_exercise, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, extractiveness, 0.0).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__study_as_exercise, mountain).
narrative_ontology:human_readable(temple_sacrifice_commitment__study_as_exercise, "Temple Sacrifice Commitment — Study as Exercise Reading").
narrative_ontology:topic_domain(temple_sacrifice_commitment__study_as_exercise, "religious/halakhic/commitment_system").

domain_priors:emerges_naturally(temple_sacrifice_commitment__study_as_exercise).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__study_as_exercise, 'e6e41644-8050-44a5-a315-8891ee837738').
narrative_ontology:cs_kernel_codification('e6e41644-8050-44a5-a315-8891ee837738', fixed_text).
narrative_ontology:cs_authority_grounding('e6e41644-8050-44a5-a315-8891ee837738', lineage).
narrative_ontology:cs_interpretation_layer_present('e6e41644-8050-44a5-a315-8891ee837738').
narrative_ontology:cs_reading_relation('e6e41644-8050-44a5-a315-8891ee837738', temple_sacrifice_commitment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('e6e41644-8050-44a5-a315-8891ee837738', temple_sacrifice_commitment__hybrid_preparatory, coexists_with).
narrative_ontology:cs_reading_relation('e6e41644-8050-44a5-a315-8891ee837738', temple_sacrifice_commitment__symbolic_transformation, influences).
narrative_ontology:cs_axiom('e6e41644-8050-44a5-a315-8891ee837738', foundational, study_constitutes_performance).
narrative_ontology:cs_axiom_status(study_constitutes_performance, holdable).
narrative_ontology:cs_axiom_grounding('e6e41644-8050-44a5-a315-8891ee837738', study_constitutes_performance, deontological).
narrative_ontology:cs_axiom('e6e41644-8050-44a5-a315-8891ee837738', foundational, intellectual_engagement_covenant_fidelity).
narrative_ontology:cs_axiom_status(intellectual_engagement_covenant_fidelity, holdable).
narrative_ontology:cs_axiom_grounding('e6e41644-8050-44a5-a315-8891ee837738', intellectual_engagement_covenant_fidelity, conventional).
narrative_ontology:cs_reference_frame('e6e41644-8050-44a5-a315-8891ee837738', study_is_performance).
narrative_ontology:cs_drift_state('e6e41644-8050-44a5-a315-8891ee837738', contemporary_halakhic_authority, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e6e41644-8050-44a5-a315-8891ee837738', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__study_as_exercise, studying_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engaged in textual and oral study of sacrifice law as a constitutive practice of covenant maintenance. Within this reading's framework, the study itself IS the performance of the divine command — intellectual engagement with halakhic texts occupies the commitment structurally and spiritually in the absence of material sacrifice conditions (temple destroyed, exile condition, restoration awaited). The community sustains the commitment through continuous interpretive and scholarly work. No coercion is present; participation is voluntary within the committed tradition.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, studying_community, beneficiary,
    analytical, civilizational, analytical, universal).

% Transmits and certifies the halakhic interpretation of study-as-performance through institutional structures (yeshivas, ordination, textual commentary tradition). Sets the curriculum of study, determines which texts are canonical, and guides interpretive direction. Within this reading, rabbinical authority is a conduit for the lineage transmission of the divine command, not an extraction mechanism — the authority serves the community's engagement with the tradition.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, rabbinical_authority, agenda_setter,
    institutional, civilizational, mobile, universal).

% The corpus of Mishnaic, Talmudic, and halakhic literature on sacrifice (primarily Seder Kodashim) remains the binding kernel of this commitment. The tradition is transmitted through study and oral interpretation, not through material practice. Its authority grounds the claim that engagement with it constitutes performance.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, textual_tradition, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_commitment__study_as_exercise, textual_tradition).

% The foundational axiom of this reading: study of the sacrifice laws is itself a form of executing the divine command. This reading holds that intellectual occupation of halakhic systems can constitute actual performance in the absence of material conditions.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, divine_command, observer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(temple_sacrifice_commitment__study_as_exercise, divine_command).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_commitment__study_as_exercise, diffuse).
narrative_ontology:fixing_cost_class(temple_sacrifice_commitment__study_as_exercise, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the halakhic study of sacrifice law as the continuing exercise of the covenant commitment in the absence of material sacrifice conditions (post-temple destruction and ongoing exile). The studying community collectively sustains the textual tradition and its interpretive continuity, ensuring the divine command remains occupied and active.
% TRANSFER_FUNCTION: This reading presents zero extractive transfer between agents. All participants in the study benefit equally from covenant maintenance and spiritual occupation of the divine command. The tradition flows from ancient texts through the lineage of interpretation to contemporary scholars; all occupy the same position of fidelity. No asymmetric distribution or wealth transfer occurs.
% ABSENT_VOICES: Practitioners of the performance_only reading would argue that study without material sacrifice is incomplete and should not be framed as full performance of the command. They are present as a dissenting position within the halakhic tradition itself but are marginalized in institutional authority structures. A literalist sect demanding material sacrifice restoration, if it existed, would also contest this reading's adequacy. Some contemporary Jewish theologians who view the study-as-performance claim as a post-hoc rationalization of institutional necessity would also object.
% DISAPPEARANCE_RATIONALE: If this constraint (the claim that study IS performance) were to disappear — if the studying community ceased to hold that intellectual engagement occupies the commitment — the halakhic corpus would persist as archival text but its authority as a living, performative practice would be fundamentally contested. The commitment's status would shift: is it actively maintained or merely preserved? The sibling readings would surface as genuine alternatives rather than intramural disputations within a settled framework. Different institutional and theological consequences would follow: if study is not performance, does the covenant go unexercised? The world would not physically rearrange, but the commitment's spiritual status would be radically altered.
% FOUNDING_PROBLEM: After the temple's destruction in 70 CE, how does the covenant commitment persist when its material instantiation (sacrifice) is impossible? The divine command to offer sacrifice bound the Jewish people as a central obligation, yet the physical conditions for performing that command were removed from the world. How does a community maintain its binding obligation to a divine command whose performance conditions have been severed?
% FOUNDING_PROBLEM_CORROBORATION: This reading is corroborated by the entire medieval and early-modern halakhic tradition (Rambam, Shulchan Aruch, and contemporary Orthodox scholarship), which treats study of sacrifice law as obligatory and as spiritually equivalent to performance. The claim is not merely self-asserted by beneficiaries — it is encoded in binding legal texts (Talmud Menachot 110a, Rambam's Mishneh Torah laws of the Temple), sustained by institutional study structures (daily learning obligations in yeshiva culture), and affirmed by authoritative halakhic codes. Independent scholarly sources (Sacks on covenant, Kaplan on halakhic philosophy, Katz on Jewish medieval thought) recognize this as the dominant classical reading in post-temple Judaism. However, dissenters exist: Karaite theological critique (rejecting the oral tradition's hermeneutic authority), some modern Jewish philosophy questioning whether the claim's adequacy, and contemporary scholars (Urbach, Baumgarten) examining whether this reading obscures the problem of commandments that cannot be performed. Corroboration is therefore substantial but not universal.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__study_as_exercise, contested).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__study_as_exercise, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__study_as_exercise, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__study_as_exercise, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__study_as_exercise_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, ExtMetricName, E),
    domain_priors:suppression_score(temple_sacrifice_commitment__study_as_exercise, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(temple_sacrifice_commitment__study_as_exercise),
    narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(temple_sacrifice_commitment__study_as_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is zero: no agent extracts from another. The studying community benefits (covenant fidelity, intellectual continuity), but this benefit is not asymmetrically distributed or coercively enforced. All participants in the study occupy the same structural position. Suppression is zero: the reading requires no coercive machinery — it relies on authority (lineage, textual tradition) and voluntary participation. Theater is zero: study is not performative activity masking a different function; it is presented as genuinely constitutive of the commitment. Accessibility_collapse is very high (0.95) because once the reading is understood (study IS performance), no alternatives to this interpretive framework collapse readily — the logic is internally consistent and grounded in binding texts. Resistance is very low (0.05) because within the studying community that affirms this reading, there is minimal resistance to the claim; the resistance comes from outside (the performance_only reading), not from within. The measurement series remain flat across the interval because this reading presents the constraint as a structural logical consequence of the commitment, not as a variable institutional arrangement. If the reading were true, we would expect zero drift. Any measured drift would suggest the reading's authority is not as invariant as claimed — an omega question.
 *
 * PERSPECTIVAL GAP:
 *   This constraint is claimed as a mountain. If genuine, it should present identical structure from every seat: study IS performance, the commitment is maintained, no extraction occurs. A true mountain has no perspectival divergence. However, the performance_only reading would compute this very differently from its seat — study would be archival, not performance; no commitment would be maintained; possible institutional extraction through gatekeeping control. The fact that two readings produce radically divergent computations suggests the kernel contest is not settled, and the mountain claim may reflect the authority position rather than a natural law. This is exactly what omega_2 (beneficiary_authenticity_mountain_check) is designed to surface: whether institutional control of the textual tradition creates asymmetries that would reclassify the constraint from mountain to extraction. The engine computes per-seat types; if this reading's seat and the performance-only reading's seat diverge sharply, the mountain claim is either false or requires FSM analysis.
 *
 * DIRECTIONALITY LOGIC:
 *   All agents within this reading occupy the same directionality: d near the beneficiary end (the covenant is maintained, the commitment is fulfilled). There is no payer, no target. This flat directionality profile is consistent with a genuine mountain or coordination mechanism with no extraction. However, if institutional gatekeeping of halakhic authority exists (rabbinical certification, yeshiva hierarchies, control of who may teach), a hidden target might exist: the studying community that lacks institutional recognition. This would shift some directionality values upward and suggest hidden asymmetry. Directionality_overrides are not needed for this story because the structural data (zero beneficiaries, zero victims, zero enforcement) all point to the same d profile. If FSM analysis reveals institutional capture, overrides might become necessary in a reclassified version.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how does the covenant persist when material sacrifice is impossible?) remains live — the studying community affirms that the commitment is actively maintained through textual engagement. The disappearance verdict is contested because the sibling readings dispute whether study truly occupies the commitment or merely preserves it. If the study_as_exercise reading were true and universally recognized, the disappearance_verdict would be 'world_rearranges' (cessation of study would break the covenant). If the performance_only reading were true, the disappearance_verdict would be 'world_unchanged' (study is archival; its cessation would preserve the corpus but not affect commitment). The fact that the verdict is contested signals that the kernel is not settled and this constraint's type is not finalized by natural law — it is contestable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    performance_constitution_ambiguity,
    'Is study of sacrifice law truly a form of performance of the divine command, or is it a substitution for performance in the absence of material conditions?',
    'This is a CONCEPTUAL omega rooted in the contested kernel itself. Resolution depends on which theological and halakhic axiom one adopts: does ''performance'' denote material instantiation only, or can intellectual engagement constitute actual performance? No empirical test can resolve whether intellectual engagement IS actual performance or merely TAKES THE PLACE OF it. The question is internal to the commitment system''s self-understanding and to how one reads the hermeneutical tradition.',
    'If study is deemed true performance (this reading''s foundational axiom study_constitutes_performance), the constraint shows zero extractiveness and the studying community occupies the full commitment, maintaining covenant fidelity through intellectual engagement. If study is deemed substitution or preparation (the performance_only or hybrid_preparatory readings), the constraint''s type shifts, potential extraction may surface, and the covenant''s maintenance becomes ambiguous. The sibling readings differ precisely on this axiom.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(performance_constitution_ambiguity, conceptual, 'Whether study constitutes actual performance of the divine command or substitutes for it — a kernel-level disagreement not resolvable by external empirical data.').

omega_variable(
    beneficiary_authenticity_fsm_candidate,
    'Does declaring the studying community as a beneficiary on a claimed mountain conceal institutional extraction through rabbinical control of halakhic interpretation and access to textual study?',
    'False-Summit Mountain (FSM) detection: examine whether institutional gatekeeping of halakhic interpretation (rabbinical ordination, yeshiva admission hierarchies, certification of qualified scholars, control of commentary tradition) extracts benefit asymmetrically. If textual study is truly open, accessible, and all participants occupy equivalent commitment positions, the mountain claim and beneficiary declaration hold. If institutional authority controls who may study, how texts are interpreted, or who may transmit the tradition with authority, an extractive power structure may exist beneath the mountain and beneficiary framing.',
    'If institutional capture is present, this constraint reclassifies from mountain to tangled_rope or snare: rabbinical authority becomes agenda_setter, the studying community segments into certified (beneficiaries) and uncertified (payers), and the covenant commitment is vindicated but not beneficiary-level. The natural-law framing masks institutional asymmetry. The FSM trigger (beneficiary present on mountain) requires at least one omega documenting the uncertainty — this omega serves that function. If study is genuinely open and non-extractive, the mountain classification and zero extractiveness remain valid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_authenticity_fsm_candidate, empirical, 'Whether institutional gatekeeping of halakhic authority creates asymmetric extraction that reclassifies the constraint from mountain to extraction.').

omega_variable(
    temporal_drift_study_framing_authority,
    'Has the halakhic tradition''s framing of study-as-performance remained stable and dominant across the 2000-year interval, or has it been contested, supplemented, or displaced by alternative readings gaining institutional weight?',
    'Historical textual and institutional analysis tracking how medieval, early-modern, and contemporary halakhic sources describe the status of sacrifice-law study. Look for moments where: (1) the claim is challenged (e.g., performance_only critique surfaces); (2) alternatives are elevated to co-equal status (prayer elevated to equal or primary weight alongside study); (3) the framing is revised (study reframed as preparatory rather than occupying); (4) institutional gatekeeping intensifies (certification requirements tighten, restricting who may teach). These would signal drift from a stable natural law to a contested negotiation.',
    'If the study-as-performance claim has remained stable and institutionally dominant (as the mountain thesis asserts), the constraint maintains its zero-extractiveness and mountain status across the interval. If the claim has been substantially contested or displaced (by prayer''s ascendance, Kabbalah''s mystical alternatives, modern Jewish theology''s critiques), this reading may have drifted from natural-law-like inevitability to one voice among institutionally weighted alternatives — a shift from certified mountain to contested rope or piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporal_drift_study_framing_authority, empirical, 'Whether study-as-performance is a stable, persistent feature of halakhic authority or has undergone contestation and displacement.').

omega_variable(
    axiom_foreclosure_logic,
    'Do the study_as_exercise axiom (study_constitutes_performance) and the performance_only axiom (study does not constitute performance, only archives) logically foreclose each other, or can both remain live within different institutional frameworks?',
    'CONCEPTUAL omega examining the logical structure of the kernel''s axioms. If ''performance'' has a univocal meaning in halakhic discourse, the axioms foreclose: one reading must be false. If ''performance'' can be framed differently in each (e.g., ''material performance'' vs. ''spiritual/covenantal performance''), both readings can coexist as different interpretations of the same ambiguous term. Examine halakhic texts to determine whether the term''s meaning is fixed or admits multiple coherent readings.',
    'If the axioms foreclose each other, the reading_relations field correctly uses ''forecloses'' and the kernel contest is genuinely bipolar (one must lose authority). If they coexist through reframing ''performance'', the relation should be ''coexists_with'' and both readings remain institutionally live. The resolution of this omega changes the network topology and determines whether this reading''s dominance blocks the performance-only reading or permits both.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(axiom_foreclosure_logic, conceptual, 'Whether the foundational axioms of study_as_exercise and performance_only readings logically foreclose or semantically coexist.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__study_as_exercise, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 0, 0.0).
narrative_ontology:measurement_basis(temp_tr_t0, observed).
narrative_ontology:measurement(temp_tr_t500, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 500, 0.0).
narrative_ontology:measurement_basis(temp_tr_t500, observed).
narrative_ontology:measurement(temp_tr_t1000, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 1000, 0.0).
narrative_ontology:measurement_basis(temp_tr_t1000, observed).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 1500, 0.0).
narrative_ontology:measurement_basis(temp_tr_t1500, observed).
narrative_ontology:measurement(temp_tr_t2000, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 2000, 0.0).
narrative_ontology:measurement_basis(temp_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 0, 0.0).
narrative_ontology:measurement_basis(temp_be_t0, observed).
narrative_ontology:measurement(temp_be_t500, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 500, 0.0).
narrative_ontology:measurement_basis(temp_be_t500, observed).
narrative_ontology:measurement(temp_be_t1000, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 1000, 0.0).
narrative_ontology:measurement_basis(temp_be_t1000, observed).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 1500, 0.0).
narrative_ontology:measurement_basis(temp_be_t1500, observed).
narrative_ontology:measurement(temp_be_t2000, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 2000, 0.0).
narrative_ontology:measurement_basis(temp_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 0, 0.0).
narrative_ontology:measurement_basis(temp_su_t0, observed).
narrative_ontology:measurement(temp_su_t500, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 500, 0.0).
narrative_ontology:measurement_basis(temp_su_t500, observed).
narrative_ontology:measurement(temp_su_t1000, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 1000, 0.0).
narrative_ontology:measurement_basis(temp_su_t1000, observed).
narrative_ontology:measurement(temp_su_t1500, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 1500, 0.0).
narrative_ontology:measurement_basis(temp_su_t1500, observed).
narrative_ontology:measurement(temp_su_t2000, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 2000, 0.0).
narrative_ontology:measurement_basis(temp_su_t2000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__study_as_exercise, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_commitment__study_as_exercise, 0.05).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__hybrid_preparatory).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the contested kernel temple_sacrifice_commitment. The kernel concerns how the divine command to sacrifice persists after the temple's destruction. All four readings affect each other because each reading's coherence depends partly on what the sibling readings claim. The study_as_exercise reading forecloses (or coexists with, depending on axiom interpretation — see omegas) the performance_only reading; it influences the hybrid and symbolic readings by staking a claim about what study's status is. All four files must link via network.affects_constraints to model the kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
