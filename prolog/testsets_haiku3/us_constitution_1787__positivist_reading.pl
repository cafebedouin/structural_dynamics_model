% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__positivist_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: us_constitution_1787__positivist_reading
 *   human_readable: Constitutional Meaning as Text Plus Democratic Amendment (Positivist Reading)
 *   domain: legal/political
 *
 * SUMMARY:
 *   The positivist reading of the U.S. Constitution holds that constitutional
 *   meaning consists of what the text actually says—interpreted according to
 *   its plain public meaning—plus any formally ratified amendments. Judicial
 *   interpretation is constrained to this domain; judges cannot invent new
 *   meaning by reinterpreting language beyond what it supports. When the
 *   Constitution's fixed meaning diverges from evolving social values, the
 *   legitimate mechanism for change is the amendment process, not judicial
 *   reinterpretation. This reading sits between originalism (which anchors
 *   meaning to the framers' historical intent) and living constitutionalism
 *   (which permits judges to evolve meaning to reflect contemporary
 *   understanding). The positivist reading is text-bound but not
 *   history-bound; it permits amendment but forbids interpretive overreach.
 *
 * KEY AGENTS:
 *   - Textualist judges administering the text-bound reading through constitutional interpretation
 *   - Amendment-process participants (Congress, state legislatures, the public) holding the legitimate power to update meaning
 *   - Living-constitutionalist advocates constrained by the reading's boundary on judicial discretion
 *   - Originalist advocates sharing text-fidelity but disagreeing on whether original intent binds
 *   - Supermajority coalitions excluded from the amendment path and barred from judicial update
 *   - Constitutional scholars analyzing the reading's coherence and empirical effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__positivist_reading, 0.31).
domain_priors:suppression_score(us_constitution_1787__positivist_reading, 0.22).
domain_priors:theater_ratio(us_constitution_1787__positivist_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, extractiveness, 0.31).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__positivist_reading, rope).
narrative_ontology:human_readable(us_constitution_1787__positivist_reading, "Constitutional Meaning as Text Plus Democratic Amendment (Positivist Reading)").
narrative_ontology:topic_domain(us_constitution_1787__positivist_reading, "legal/political").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__positivist_reading, '67e8ecb7-f81d-4f10-98ea-92227c148d2b').
narrative_ontology:cs_kernel_codification('67e8ecb7-f81d-4f10-98ea-92227c148d2b', fixed_text).
narrative_ontology:cs_authority_grounding('67e8ecb7-f81d-4f10-98ea-92227c148d2b', lineage).
narrative_ontology:cs_interpretation_layer_present('67e8ecb7-f81d-4f10-98ea-92227c148d2b').
narrative_ontology:cs_reading_relation('67e8ecb7-f81d-4f10-98ea-92227c148d2b', us_constitution_1787__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('67e8ecb7-f81d-4f10-98ea-92227c148d2b', us_constitution_1787__living_reading, coexists_with).
narrative_ontology:cs_axiom('67e8ecb7-f81d-4f10-98ea-92227c148d2b', foundational, judicial_interpretation_text_constrained).
narrative_ontology:cs_axiom_status(judicial_interpretation_text_constrained, holdable).
narrative_ontology:cs_axiom_grounding('67e8ecb7-f81d-4f10-98ea-92227c148d2b', judicial_interpretation_text_constrained, deontological).
narrative_ontology:cs_axiom('67e8ecb7-f81d-4f10-98ea-92227c148d2b', foundational, amendment_process_democratic_legitimacy).
narrative_ontology:cs_axiom_status(amendment_process_democratic_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('67e8ecb7-f81d-4f10-98ea-92227c148d2b', amendment_process_democratic_legitimacy, conventional).
narrative_ontology:cs_reference_frame('67e8ecb7-f81d-4f10-98ea-92227c148d2b', text_bound_judicial_restraint).
narrative_ontology:cs_drift_state('67e8ecb7-f81d-4f10-98ea-92227c148d2b', contemporary_constitutional_politics, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('67e8ecb7-f81d-4f10-98ea-92227c148d2b', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__positivist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, textualist_judiciary).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, amendment_process_participants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, originalist_advocates).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, state_and_local_governments).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, federal_legislature).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, living_constitutionalist_advocates).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, state_and_local_governments).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, federal_legislature).
narrative_ontology:constraint_vindicates(us_constitution_1787__positivist_reading, constitutional_supremacy_doctrine).
narrative_ontology:constraint_vindicates(us_constitution_1787__positivist_reading, judicial_restraint_principle).
narrative_ontology:constraint_vindicates(us_constitution_1787__positivist_reading, democratic_amendment_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Constitution as bound by the text's plain meaning at the time of adoption, plus any subsequently ratified amendments. Constrains its own interpretive discretion: rejects living-document reasoning that would attribute meaning the text does not support. Administers the reading through judicial decisions that apply this interpretive rule. Benefits from clear boundaries on their authority and from the legitimacy of text-fidelity.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, textualist_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Congress, state legislatures, and the public acting through the amendment process. Under this reading, the amendment process is the primary democratic mechanism for updating constitutional meaning when text proves inadequate. They benefit from holding the legitimate power to reshape the constitution, but bear the high cost of achieving supermajority agreement across federal and state levels.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, amendment_process_participants, beneficiary,
    organized, generational, constrained, national).

% Scholars, judges, and political actors committed to interpreting the Constitution according to original public meaning (the meaning the text held at ratification). The positivist reading shares their text-fidelity and historical constraint but diverges on whether original intent binds or merely informs. They benefit from any move away from living constitutionalism but may see this reading as still insufficiently historically grounded.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, originalist_advocates, beneficiary,
    powerful, generational, mobile, national).

% Scholars, judges, and political actors who argue the Constitution's meaning must evolve to reflect changing social values and understanding. The positivist reading constrains their interpretive freedom: they can no longer claim judicial discretion to update meaning without formal amendment. They bear the cost of having their interpretive tradition marked as outside the legitimate bounds of the reading, and must achieve constitutional amendment to effect change they believe judges could justify.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, living_constitutionalist_advocates, payer,
    powerful, generational, mobile, national).

% Political coalitions that would support a constitutional change (on civil rights, economic regulation, or other matters) but cannot meet the supermajority threshold required for amendment. They are excluded from voice in this constraint's structure: the reading makes amendment the only legitimate path, but amendment is inaccessible to them. They cannot persuade a court to update meaning without crossing the reading's boundaries.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, super_majority_coalitions, excluded,
    organized, biographical, trapped, national).

% Engage in academic analysis of constitutional law and the legitimacy of different interpretive methodologies. They analyze the reading's internal coherence, empirical track record (how well does text-plus-amendment actually constrain judicial discretion in practice?), and its relationship to competing readings. They take no collective position but their analysis shapes how the reading is understood and refined.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, constitutional_scholars, observer,
    moderate, generational, mobile, national).

% Operate under federal constitutional authority. Under this reading, they benefit from judicial restraint (judges will not invent new federal powers by reinterpreting the text), but pay the cost of amendment gridlock: legitimate constitutional changes that would serve their interests require the arduous supermajority process rather than judicial evolution. Their situation is asymmetrically constrained: they cannot exit the constitutional order but also cannot easily reshape it.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, state_and_local_governments, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__positivist_reading, state_and_local_governments, beneficiary).

% Passes legislation under the Constitution's text as interpreted by courts bound to this reading. Benefits from clear, stable boundaries on implied federal powers (judges will not invent constitutional authority Congress does not have). Pays the cost of amendment participation: to extend federal authority beyond the text requires supermajority constitutional amendment, not merely a sympathetic court majority.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, federal_legislature, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__positivist_reading, federal_legislature, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a common framework for interpreting fundamental law: what counts as a legitimate constitutional meaning, and through what mechanism can that meaning change? The reading coordinates by providing judges with a clear rule (text plus amendments) rather than discretionary balancing, which reduces forum-shopping and makes constitutional law more predictable across jurisdictions and over time.
% TRANSFER_FUNCTION: Transfers interpretive authority from courts (under living constitutionalism) to the amendment process (requiring supermajority democratic action). Meaning-making power moves from a concentrated institutional seat (the judiciary) to a dispersed, high-friction democratic mechanism. The cost of constitutional change is raised for all parties, and the cost structure is symmetric: no faction can unilaterally update the constitution without supermajority support.
% ABSENT_VOICES: Supermajority coalitions that cannot meet the amendment threshold have no voice in the reading's structure: they cannot appeal to courts to evolve the constitution to accommodate them, and they are locked out of the formal amendment path. Their absence from the negotiation table is structural, not accidental. Also absent: international or transnational constitutional voices, which the text-bound reading explicitly excludes.
% DISAPPEARANCE_RATIONALE: If this reading were supplanted by living constitutionalism, constitutional law would reorganize: courts would recover discretion to interpret the constitution in light of contemporary values; the amendment process would diminish in importance as a change mechanism; judicial activism would no longer face the constraint of text-fidelity; the meaning of the constitution would be contestable and dynamic rather than fixed and stable. The political economy of constitutional litigation would fundamentally shift.
% FOUNDING_PROBLEM: The Constitution is a fixed text whose meaning can diverge from evolving social values, creating pressure for courts to 'discover' updated meaning by reinterpreting the text beyond what it says. This pressure creates two opposite temptations: judges either twist the text to reach contemporary results (living constitutionalism), or judges cling to original historical meaning even when it produces results no current supermajority supports (originalism). A middle path: constrain judges to what the text says (no twisting), but allow democratic amendment to update the text itself (no originalist lock-in).
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars across the ideological spectrum (including critics of the reading) acknowledge that constitutional meaning and social values periodically misalign, and that courts face pressure to bridge the gap. Originalist scholars attest the problem of interpretive overreach. Living constitutionalist scholars attest the problem of amendment gridlock. The bifurcation between text-bound interpretation and democratic amendment is attested from outside the reading's beneficiary seats, even by scholars who reject the reading as incomplete.
narrative_ontology:disappearance_verdict(us_constitution_1787__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__positivist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__positivist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(us_constitution_1787__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__positivist_reading, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__positivist_reading_tests).
:- end_tests(us_constitution_1787__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-to-moderate (0.31 at interval end) because the reading imposes a genuine coordination benefit (stable, predictable constitutional meaning) while also raising the cost of change (amendment gridlock). The constraint does extract from those locked out of supermajority coalitions, but the extraction is transparent and structural rather than hidden or coercive. Suppression is low (0.22) because the reading's legitimacy is contested (living constitutionalists actively argue against it) and enforcement relies on judicial self-restraint and academic consensus rather than coercive exclusion. Theater is low (0.18) because the reading's stated function (constraining judges to the text) is genuine, though judges' behavior imperfectly matches the reading's aspirations. The measurement series track a slow rise in extractiveness over the interval as amendment gridlock intensifies and the cost of change becomes clearer.
 *
 * PERSPECTIVAL GAP:
 *   A textualist judge experiences this reading as liberating (clear rule, reduced discretion, increased legitimacy). A living-constitutionalist judge experiences it as imprisoning (constrained by text they believe is inadequate, unable to evolve meaning). A legislator in a supermajority coalition experiences it as empowering (their votes matter for constitutional change). A legislator in a blocked minority experiences it as disempowering (supermajority requirement is insurmountable). The engine should compute different types from these different seats: the judge may compute the reading as genuine rope (coordination), while the legislator in a blocked minority computes it as snare (extraction without exit). The asymmetry is structural, not a measurement artifact.
 *
 * DIRECTIONALITY LOGIC:
 *   Textualist judges benefit by gaining clear interpretive authority and the legitimacy of text-fidelity, positioning them as agenda-setters with d near 0.2 (low extraction). Amendment-process participants benefit theoretically but pay the cost of high barriers to change; they sit near 0.45 (symmetric). Living-constitutionalist advocates are the primary targets—their interpretive freedom is constrained—positioning them at d ≈ 0.75 (high extraction). Supermajority coalitions unable to meet the amendment threshold experience pure constraint with no compensatory benefit, approaching d ≈ 0.85 (full target). State and federal governmental seats are asymmetrically positioned: they benefit from judicial restraint but pay for amendment gridlock.
 *
 * MANDATROPHY ANALYSIS:
 *   The reading claims to solve a coordination problem (how should judges interpret the Constitution?) by imposing a rule (text plus amendment). The mandatrophy concern is whether the founding problem it addresses—interpretive overreach by living-constitutionalist judges—remains live or has been displaced by a new problem: amendment gridlock. Originalist critics argue the reading solves neither problem (they claim original intent is what constrains interpretation). Living-constitutionalist critics argue the reading creates a worse problem (it locks out legitimate constitutional evolution when supermajorities cannot be formed). The reading's persistence depends on whether textualist judges maintain restraint and whether the academic and political consensus supports text-bound interpretation. If living constitutionalism resurges among judges, the reading's constraint collapses. The theater metric's slow rise suggests increasing performative maintenance: judges invoke 'text-bound' language while making discretionary moves, or the reading is sustained more by institutional inertia than by genuine commitment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    judicial_self_restraint_sustainability,
    'Can judges reliably maintain text-bound interpretation without slipping into reinterpreting the text to reach preferred contemporary outcomes?',
    'Longitudinal analysis of judicial opinion language: do judges citing ''plain meaning'' actually constrain their reasoning to what the text supports, or do they use textual language as cover for value-driven interpretation? Do opinion patterns differ systematically between textualist and non-textualist judges on the same cases?',
    'If judicial restraint proves unsustainable and judges systematically reinterpret the text beyond what it supports while claiming text-fidelity, the reading degrades into a performance masking living constitutionalism. The effective constraint becomes a snare (extracting legitimacy from the appearance of restraint while denying it in practice). If restraint proves sustainable, the reading operates as intended.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_self_restraint_sustainability, empirical, 'Whether judges can sustain genuine text-bound interpretation or whether the reading becomes a cover story for reinterpreted constitutional meaning.').

omega_variable(
    amendment_gridlock_vs_coordination,
    'Does the amendment requirement enable or block legitimate democratic constitutional change? Is the high barrier a feature (ensuring stable constitutional meaning) or a bug (locking supermajority coalitions out of constitutional power)?',
    'Counterfactual historical analysis: which amendment-eligible constitutional changes (policies that could have been constitutionally embedded) failed to achieve amendment and why? Did failure result from lack of supermajority support, or from structural barriers (federalism, veto players) independent of the amendment rule? Comparison to constitutions with lower amendment barriers: do they exhibit constitutional meaning drift, or stable meaning with easier change?',
    'If amendment barriers predominantly block changes that supermajorities would support if the barriers were lower, the constraint functions as snare (extracting stability from excluded coalitions). If barriers reliably reflect genuine absence of supermajority support, the constraint functions as rope (enforcing stability through authentic democratic consensus). The classification could diverge significantly across different historical moments and policy domains.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(amendment_gridlock_vs_coordination, empirical, 'Whether amendment gridlock reflects democratic consensus or structural exclusion of legitimate supermajority preferences.').

omega_variable(
    originalism_vs_positivism_boundary,
    'Does this reading actually diverge from originalism in practice, or do text-bound interpretation and original-intent interpretation reach identical conclusions in most cases?',
    'Comparative jurisprudence: identify cases where a strict originalist reading would produce a different result from a positivist (text-bound, non-historical) reading. Do such cases exist? How frequently? Do the readings diverge more or less than positivist and living-constitutionalist readings diverge?',
    'If the readings prove empirically identical (text-bound and original-intent consistently converge), then the positivist reading is a conceptual rather than practical alternative, and its distinction from originalism is rhetorical rather than structural. If they regularly diverge (especially on historical amendments and contemporary statutory interpretation of constitutional terms), the readings are genuinely separate constraints with different extraction profiles. This affects whether originalist judges can coherently adopt the positivist frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(originalism_vs_positivism_boundary, empirical, 'Whether positivist and originalist readings are empirically distinct or converge in practice.').

omega_variable(
    amendment_process_democratic_legitimacy,
    'Does the amendment process—requiring ratification by three-fourths of state legislatures—actually reflect democratic will, or does it create veto-player dynamics that block changes with majority (but not supermajority) support?',
    'Public opinion polling on hypothetical amendments: where the reading claims the amendment process is the legitimate path to constitutional change, do supermajorities actually exist for constitutional changes that courts have refused to enact? Conversely, where courts have enacted changes via reinterpretation, would a formal amendment pass if put to the amendment process?',
    'If amendment-eligible changes with supermajority public support are blocked by federalism veto-players (small states, geographic distribution of preferences), the reading''s claim that amendment is the democratic mechanism becomes questionable—the reading extracts from dispersed majorities for the sake of supermajority minorities. If supermajorities genuinely support the boundaries the reading enforces, the reading operates as rope. The classification is highly sensitive to this omega''s resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(amendment_process_democratic_legitimacy, empirical, 'Whether the amendment process reflects genuine democratic consensus or structural veto-player exclusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__positivist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t0, us_constitution_1787__positivist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(us_c_tr_t0, observed).
narrative_ontology:measurement(us_c_tr_t10, us_constitution_1787__positivist_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement_basis(us_c_tr_t10, observed).
narrative_ontology:measurement(us_c_tr_t20, us_constitution_1787__positivist_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement_basis(us_c_tr_t20, observed).
narrative_ontology:measurement(us_c_tr_t30, us_constitution_1787__positivist_reading, theater_ratio, 30, 0.17).
narrative_ontology:measurement_basis(us_c_tr_t30, observed).
narrative_ontology:measurement(us_c_tr_t40, us_constitution_1787__positivist_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(us_c_tr_t40, observed).
narrative_ontology:measurement(us_c_tr_t50, us_constitution_1787__positivist_reading, theater_ratio, 50, 0.18).
narrative_ontology:measurement_basis(us_c_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(us_c_be_t0, us_constitution_1787__positivist_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(us_c_be_t0, observed).
narrative_ontology:measurement(us_c_be_t10, us_constitution_1787__positivist_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement_basis(us_c_be_t10, observed).
narrative_ontology:measurement(us_c_be_t20, us_constitution_1787__positivist_reading, base_extractiveness, 20, 0.26).
narrative_ontology:measurement_basis(us_c_be_t20, observed).
narrative_ontology:measurement(us_c_be_t30, us_constitution_1787__positivist_reading, base_extractiveness, 30, 0.29).
narrative_ontology:measurement_basis(us_c_be_t30, observed).
narrative_ontology:measurement(us_c_be_t40, us_constitution_1787__positivist_reading, base_extractiveness, 40, 0.3).
narrative_ontology:measurement_basis(us_c_be_t40, observed).
narrative_ontology:measurement(us_c_be_t50, us_constitution_1787__positivist_reading, base_extractiveness, 50, 0.31).
narrative_ontology:measurement_basis(us_c_be_t50, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(us_constitution_1787__positivist_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__positivist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(us_constitution_1787__positivist_reading, 0.12).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, us_constitution_1787__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, us_constitution_1787__living_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested kernel 'us_constitution_1787'. The originalist reading fixes meaning at ratification; the living reading permits judicial evolution. This positivist reading permits amendment but forbids interpretive overreach. All three readings share the same text (the 1787 Constitution and its amendments) but instantiate different constraints through different interpretive rules and justificatory frameworks. The readings form a kernel family linked by affecting_constraints edges. They are not hierarchical or sequential—they coexist as live positions in contemporary constitutional discourse, held by different institutional and scholarly actors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_1787__positivist_reading, powerful, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
