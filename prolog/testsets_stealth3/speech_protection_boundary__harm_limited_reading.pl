% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__harm_limited_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__harm_limited_reading, []).

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
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: speech_protection_boundary__harm_limited_reading
 *   human_readable: Harm-Limited Speech Protection Boundary (Dignity/Equality/Harassment Conditioning)
 *   domain: legal/political philosophy
 *
 * SUMMARY:
 *   This story instantiates the harm_limited_reading of the
 *   speech_protection_boundary kernel: a regime in which expression receives
 *   constitutional protection conditional on its causing no significant harm
 *   to dignity, equality, or freedom from harassment. The regime solves a
 *   real coordination problem — targeted harassment silences whole classes of
 *   participants whom no individual victim can protect alone — while
 *   concentrating adjudicative discretion in state institutions whose
 *   category lists, budgets, and jurisdiction grow with each recognized harm.
 *   Speakers bear the costs through sanction and, more diffusely, through
 *   anticipatory self-censorship that never enters any enforcement record.
 *   Per the epsilon-invariance principle, the sibling readings of this kernel
 *   (absolutist_reading, balancing_reading) are separate constraint files
 *   with their own epsilon values and stakeholder structures; this file
 *   authors only the harm-conditioned regime and hedges nothing across
 *   readings.
 *
 * KEY AGENTS:
 *   - historically_targeted_groups: Primary beneficiary (organized/constrained) — receives enforceable protection of equal standing
 *   - state_harm_adjudicators: Agenda setter (institutional/constrained) — administers the boundary and collects adjudicative power as categories multiply
 *   - sanctioned_speakers: Formal target (moderate/trapped) — bears penalties after adverse adjudication
 *   - chilled_borderline_speakers: Diffuse target (moderate/constrained) — bears anticipatory self-censorship invisible to enforcement statistics
 *   - professional_expressive_classes: Structurally exposed target (organized/identity_locked) — vocationally fused with expression, carries compliance overhead and widest chilling exposure
 *   - dissenting_minority_speakers: Abuse-exposed target (powerless/trapped) — group-internal dissent recast as dignitary harm
 *   - general_public: Incidental beneficiary and diffuse payer (moderate/mobile) — receives the scrubbed discourse environment, pays taxes and lost access
 *   - civil_liberties_advocates: Analytical observer (organized/analytical) — contests threshold expansion and supplies the external drift record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__harm_limited_reading, 0.56).
domain_priors:suppression_score(speech_protection_boundary__harm_limited_reading, 0.64).
domain_priors:theater_ratio(speech_protection_boundary__harm_limited_reading, 0.36).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, theater_ratio, 0.36).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(speech_protection_boundary__harm_limited_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__harm_limited_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__harm_limited_reading, "Harm-Limited Speech Protection Boundary (Dignity/Equality/Harassment Conditioning)").
narrative_ontology:topic_domain(speech_protection_boundary__harm_limited_reading, "legal/political philosophy").

domain_priors:requires_active_enforcement(speech_protection_boundary__harm_limited_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__harm_limited_reading, 'e18c92e6-7544-4ff0-adb0-007993554630').
narrative_ontology:cs_kernel_codification('e18c92e6-7544-4ff0-adb0-007993554630', fixed_text).
narrative_ontology:cs_authority_grounding('e18c92e6-7544-4ff0-adb0-007993554630', lineage).
narrative_ontology:cs_interpretation_layer_present('e18c92e6-7544-4ff0-adb0-007993554630').
narrative_ontology:cs_reading_relation('e18c92e6-7544-4ff0-adb0-007993554630', speech_protection_boundary__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('e18c92e6-7544-4ff0-adb0-007993554630', speech_protection_boundary__balancing_reading, coexists_with).
narrative_ontology:cs_axiom('e18c92e6-7544-4ff0-adb0-007993554630', foundational, equal_standing_limits_expression).
narrative_ontology:cs_axiom_status(equal_standing_limits_expression, holdable).
narrative_ontology:cs_axiom_grounding('e18c92e6-7544-4ff0-adb0-007993554630', equal_standing_limits_expression, deontological).
narrative_ontology:cs_axiom('e18c92e6-7544-4ff0-adb0-007993554630', secondary, harm_boundary_is_justiciable).
narrative_ontology:cs_axiom_status(harm_boundary_is_justiciable, holdable).
narrative_ontology:cs_axiom_grounding('e18c92e6-7544-4ff0-adb0-007993554630', harm_boundary_is_justiciable, instrumental).
narrative_ontology:cs_reference_frame('e18c92e6-7544-4ff0-adb0-007993554630', dignitarian_equal_standing_order).
narrative_ontology:cs_drift_state('e18c92e6-7544-4ff0-adb0-007993554630', contemporary_threshold_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e18c92e6-7544-4ff0-adb0-007993554630', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__harm_limited_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, historically_targeted_groups).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, state_harm_adjudicators).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, general_public).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, sanctioned_speakers).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, chilled_borderline_speakers).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, professional_expressive_classes).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, dissenting_minority_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__harm_limited_reading, professional_expressive_classes).
narrative_ontology:constraint_victim(speech_protection_boundary__harm_limited_reading, general_public).
narrative_ontology:constraint_vindicates(speech_protection_boundary__harm_limited_reading, equal_standing_doctrine).
narrative_ontology:constraint_vindicates(speech_protection_boundary__harm_limited_reading, dignitarian_constitutionalism).
narrative_ontology:constraint_vindicates(speech_protection_boundary__harm_limited_reading, structural_subordination_thesis_of_group_directed_speech).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of racial, ethnic, religious, gender, and sexual minorities whose equal standing in public discourse the regime secures. They receive enforceable protection from expression that targets group identity, and advocacy organizations among them participate in shaping how the harm categories are drawn. Exiting would mean withdrawing from public life entirely, so they remain inside the discourse environment the boundary governs.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, historically_targeted_groups, beneficiary,
    organized, generational, constrained, national).

% Human rights commissions, speech tribunals, prosecutors, and courts that administer the harm boundary. They determine which expression crosses the significance threshold, their precedent-setting moves the threshold over time, and each added harm category enlarges their mandate, budget, and jurisdiction. They are bound by constitutional structure and cannot exit the adjudicative role, but they control its day-to-day administration.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, state_harm_adjudicators, agenda_setter,
    institutional, generational, constrained, national).

% Individuals formally penalized after adjudication finds their expression significantly harmful: fines, publication orders, takedowns, employment consequences. The determination follows them through background checks and reputational memory. Exit would mean abandoning public expression altogether, which for most is not a real option.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, sanctioned_speakers, payer,
    moderate, biographical, trapped, national).

% Speakers who never face a tribunal but self-censor near the boundary because they cannot predict which expressions will be judged significantly harmful. They retire topics, soften claims, or leave contentious fields entirely. Their cost never appears in enforcement statistics because no proceeding ever records it.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, chilled_borderline_speakers, payer,
    moderate, biographical, constrained, national).

% Journalists, academics, comedians, novelists, and essayists whose vocation is expression. They carry the widest chilling exposure and the compliance overhead of editors, counsel, and mandatory conduct training, while also working in workplaces made materially safer by harassment enforcement. Leaving expression means leaving the profession and the identity built on it, so exit is effectively unthinkable.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, professional_expressive_classes, payer,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__harm_limited_reading, professional_expressive_classes, beneficiary).

% Members of protected groups who dissent from their own community's leadership or norms. Their criticism is structurally vulnerable to being recast as harm to group dignity, they lack the resources to contest adjudication, and they cannot exit the group identity that exposes them. They are the seat where gatekeeping abuse concentrates.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, dissenting_minority_speakers, payer,
    powerless, biographical, trapped, national).

% Receives a public sphere scrubbed of harassing and group-degrading expression, and pays for the enforcement apparatus through taxation while losing access to whatever contested speech the boundary removes. Individually mobile: any one member can disengage from public discourse at low personal cost, so the stakes stay diffuse and unorganized.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, general_public, beneficiary,
    moderate, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__harm_limited_reading, general_public, payer).

% Litigate test cases, publish threshold audits, and contest each proposed category expansion. They neither collect the protection nor bear the sanctions, but their activity shapes where the boundary settles and supplies the external record of threshold drift.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__harm_limited_reading, civil_liberties_advocates, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__harm_limited_reading, state_harm_adjudicators).
narrative_ontology:fixing_cost_class(speech_protection_boundary__harm_limited_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the conditions for equal participation in public discourse by removing expression that significantly damages dignity or equality or constitutes harassment, solving the collective-action problem in which targeted harassment silences entire classes of participants while no single victim can stop it alone.
% TRANSFER_FUNCTION: Moves expressive liberty from speakers whose expression crosses the harm threshold to the security of participation of targeted groups, and moves adjudicative discretion over the boundary itself to state institutions.
% ABSENT_VOICES: Anonymous and pseudonymous speakers who cannot appear in adjudication without exposing themselves; minority dissenters whose objections are pre-read as bad faith; and speakers in jurisdictions that import the framework's vocabulary without its institutional safeguards, who would object to how the categories travel.
% DISAPPEARANCE_RATIONALE: If the harm-conditioned boundary vanished overnight, currently unprotected expression would return to mainstream channels within months, participation patterns of targeted groups would shift measurably, adjudicative institutions would lose their mandates, and platform moderation would lose its legal anchor points. The speech order would reorganize around whichever settlement replaced the boundary.
% FOUNDING_PROBLEM: Mid-twentieth-century experience showed that unrestrained expression targeting vulnerable groups precedes and enables their exclusion, subordination, and physical destruction. Liberal democracies built speech regimes that condition protection on respect for the equal standing of fellow citizens.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: historical scholarship documenting the role of group-directed propaganda before mass atrocities, comparative-jurisprudence literature, and empirical studies of harassment's documented chilling effects on participation. Critics of the regime corroborate that the founding problem was and is real while disputing whether present enforcement serves it.
narrative_ontology:disappearance_verdict(speech_protection_boundary__harm_limited_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__harm_limited_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__harm_limited_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_boundary__harm_limited_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__harm_limited_reading, 0.56, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__harm_limited_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__harm_limited_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__harm_limited_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.56 at interval end: the regime's protection of genuinely harmful-speech targets is real coordination, but the significance threshold has crept outward across the interval (from racial incitement statutes toward harassment, coded dog whistles, and generalized dignitary offense), and adjudicative discretion compounds with each added category. Suppression is 0.64 and rising: the constraint's persistence depends on an enforcement apparatus that has grown from ad hoc tribunals to statutory codes integrated with platform moderation and workplace compliance machinery. Theater ratio is 0.36: a growing share of activity is performative compliance (ritual training, symbolic prosecutions, institutional speech codes enforced unevenly) alongside a still-substantive adjudicative core. Accessibility_collapse is 0.45 — alternatives persist (private fora, reframing, relocation to other jurisdictions) but the public expressive space narrows measurably once the boundary is understood. Resistance is 0.62 — organized civil-liberties litigation, political backlash, and recurring legislative repeal attempts meet every expansion. All three tracked metrics run on one shared six-point grid (1950-2025) so every metric is authored at every examined time point. The suppression_requirement series is authored deliberately: the dynamic this story traces is enforcement-capacity build-out (an enforcement ratchet, not merely shifting extraction), which the scalar base_properties.suppression alone cannot show. The claimed type (tangled_rope) and the metrics were authored independently: the claim asserts genuine coordination plus asymmetric extraction with active enforcement; the metrics describe the regime's actual operation without being tuned to any predicted engine output.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structural data. From the adjudicator seat the regime is a mandate faithfully administered — coordination it built and staffs. From the payer seats the same structure operates as conditional permission: expression is protected until an institution judges otherwise, and the judging standard moves. From the beneficiary seats it is security of participation purchased at speakers' expense. Professional expressive classes occupy both positions at once, which is why their exit is identity_locked rather than merely constrained. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive historically_targeted_groups and state_harm_adjudicators toward the subsidized end of d; victim declarations drive sanctioned_speakers, chilled_borderline_speakers, professional_expressive_classes, and dissenting_minority_speakers toward the target end, with trapped and identity_locked exit pushing them nearer full-target than mobile payers would sit. General_public holds a genuinely dual position (cleaner discourse received, taxes and lost access paid) and sits near symmetry, but the derivation chain reads the beneficiary array, so its computed d likely undershoots symmetry. No directionality_overrides are authored: overrides key on power atoms, and the only candidates sharing an atom with general_public are the moderate-power payer seats, whose d is correctly derived — correcting one would corrupt the other. The limitation is recorded here rather than papered over with a coarse override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem remains live: group-directed harassment and dignitary attack persist, and the regime's protective function is still exercised daily. The R5 mismatch consumer therefore reads status=live against verdict=world_rearranges — an aligned pair producing no capture/zombie flag. Mandatrophy is not resolved and the constraint is not a piton: the coordination function has not atrophied, and the theater ratio, while rising, still leaves the functional share dominant. The classification prevents the opposite mislabel as well: because beneficiaries and victims are both declared with active enforcement, the regime cannot be mistaken for a pure rope (its extraction is structural, not incidental) nor for a pure snare (its coordination function is genuine and independently corroborated).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the speech_protection_boundary kernel — what would the sibling readings (absolutist_reading, balancing_reading) change structurally, and where exactly is the disagreement located?',
    'Comparative authoring of the sibling files: the absolutist reading narrows the unprotected set to imminent lawless action; the balancing reading replaces the categorical harm condition with case-by-case weighing. The disagreement is located in the conditioning rule itself — which expression counts as unprotected — not in the value of expression protection.',
    'Classification is reading-indexed: the absolutist sibling should compute a near-zero extraction profile over its narrower unprotected set, and the balancing sibling a case-varying profile. Any cross-reading comparison of epsilon must treat the three files as three constraints, not three measurements of one.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: this story is the harm_limited_reading of a three-reading kernel; siblings are separate constraints.').

omega_variable(
    significance_threshold_vagueness,
    'Where does ''significant'' harm begin, and how much of the measured extraction is inherent to the conditioning versus produced by threshold creep?',
    'Longitudinal audit of adjudicated cases measuring the distribution of harm findings against the original statutory formulations; divergence between founding-era thresholds and current practice isolates the creep component.',
    'If most measured extraction traces to creep rather than the founding threshold, the regime''s core is closer to rope than the aggregate metrics suggest, and remediation is a definitional tightening rather than structural redesign.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(significance_threshold_vagueness, conceptual, 'Irreducible vagueness in the significance threshold drives the extraction trajectory.').

omega_variable(
    gatekeeper_capture_risk,
    'Does the adjudicative power get turned on speakers the regime was not built to reach — dissenters within protected groups, political opponents, satirists?',
    'Enforcement demographic audits: cross-reference who is prosecuted or sanctioned under harm standards with dissenter status, satire, and intra-group criticism, controlling for violation severity.',
    'Confirmed capture raises effective extraction for the payer seats sharply and flavors the constraint toward its snare boundary; absence of capture supports the tangled_rope reading with the gatekeeping risk priced as latent rather than realized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeper_capture_risk, empirical, 'Whether the state gatekeeping function is captured against the regime''s own constituencies.').

omega_variable(
    chilling_undermeasurement,
    'How large is the burden borne by speakers who self-censor before any adjudication, given that no enforcement statistic records them?',
    'Willingness-to-speak survey panels benchmarked against revealed enforcement data; the gap between stated reticence and recorded proceedings estimates the unrecorded payer population.',
    'A large invisible payer population means base extractiveness understates the burden on target seats; per-seat classifications for chilled speakers would shift toward the full-target end.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chilling_undermeasurement, empirical, 'Self-censorship is structurally invisible to the enforcement record that measures extraction.').

omega_variable(
    internalized_chilling_persistence,
    'Is the suppression operating on borderline speakers structural (sanction risk) or internalized (habitus of restraint that would persist if the rules relaxed)?',
    'Natural experiment from jurisdictions that loosened harm standards: if expressive behavior rebounds quickly, suppression was structural; if reticence persists across generations of speakers, it is internalized.',
    'Internalized suppression means the constraint''s effective suppression exceeds the structural measure and survives formal reform — the target carries the boundary with them after the rule changes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_chilling_persistence, empirical, 'Structural versus internalized mechanism of the chilling effect.').

omega_variable(
    cs_framing_underdetermination,
    'Is the kernel the constitutional text itself (fixed_text, lineage authority through courts) or the interpretive tradition layered above the text (distributed kernel, practice authority)?',
    'Signals favoring the text-framing: formal entrenchment, judicial review structured around canonical clauses. Signals favoring the tradition-framing: the operative boundary lives in doctrinal tests and commission practice that vary across jurisdictions sharing the same text. Audit which layer actually adjudicates contested cases.',
    'Under the tradition-framing, kernel_codification becomes distributed and authority_grounding becomes practice, changing the commitment-system classification and the drift diagnostics; the constraint-level metrics and stakeholder structure are unchanged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Two coherent framings of the same kernel produce different cs_pattern classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__harm_limited_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spbl_harm_limited_tr_t1950, speech_protection_boundary__harm_limited_reading, theater_ratio, 1950, 0.14).
narrative_ontology:measurement_basis(spbl_harm_limited_tr_t1950, observed).
narrative_ontology:measurement(spbl_harm_limited_tr_t1965, speech_protection_boundary__harm_limited_reading, theater_ratio, 1965, 0.17).
narrative_ontology:measurement_basis(spbl_harm_limited_tr_t1965, observed).
narrative_ontology:measurement(spbl_harm_limited_tr_t1980, speech_protection_boundary__harm_limited_reading, theater_ratio, 1980, 0.21).
narrative_ontology:measurement_basis(spbl_harm_limited_tr_t1980, observed).
narrative_ontology:measurement(spbl_harm_limited_tr_t1995, speech_protection_boundary__harm_limited_reading, theater_ratio, 1995, 0.26).
narrative_ontology:measurement_basis(spbl_harm_limited_tr_t1995, observed).
narrative_ontology:measurement(spbl_harm_limited_tr_t2010, speech_protection_boundary__harm_limited_reading, theater_ratio, 2010, 0.31).
narrative_ontology:measurement_basis(spbl_harm_limited_tr_t2010, observed).
narrative_ontology:measurement(spbl_harm_limited_tr_t2025, speech_protection_boundary__harm_limited_reading, theater_ratio, 2025, 0.36).
narrative_ontology:measurement_basis(spbl_harm_limited_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(spbl_harm_limited_be_t1950, speech_protection_boundary__harm_limited_reading, base_extractiveness, 1950, 0.33).
narrative_ontology:measurement_basis(spbl_harm_limited_be_t1950, observed).
narrative_ontology:measurement(spbl_harm_limited_be_t1965, speech_protection_boundary__harm_limited_reading, base_extractiveness, 1965, 0.39).
narrative_ontology:measurement_basis(spbl_harm_limited_be_t1965, observed).
narrative_ontology:measurement(spbl_harm_limited_be_t1980, speech_protection_boundary__harm_limited_reading, base_extractiveness, 1980, 0.44).
narrative_ontology:measurement_basis(spbl_harm_limited_be_t1980, observed).
narrative_ontology:measurement(spbl_harm_limited_be_t1995, speech_protection_boundary__harm_limited_reading, base_extractiveness, 1995, 0.49).
narrative_ontology:measurement_basis(spbl_harm_limited_be_t1995, observed).
narrative_ontology:measurement(spbl_harm_limited_be_t2010, speech_protection_boundary__harm_limited_reading, base_extractiveness, 2010, 0.53).
narrative_ontology:measurement_basis(spbl_harm_limited_be_t2010, observed).
narrative_ontology:measurement(spbl_harm_limited_be_t2025, speech_protection_boundary__harm_limited_reading, base_extractiveness, 2025, 0.56).
narrative_ontology:measurement_basis(spbl_harm_limited_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(spbl_harm_limited_su_t1950, speech_protection_boundary__harm_limited_reading, suppression_requirement, 1950, 0.38).
narrative_ontology:measurement_basis(spbl_harm_limited_su_t1950, observed).
narrative_ontology:measurement(spbl_harm_limited_su_t1965, speech_protection_boundary__harm_limited_reading, suppression_requirement, 1965, 0.44).
narrative_ontology:measurement_basis(spbl_harm_limited_su_t1965, observed).
narrative_ontology:measurement(spbl_harm_limited_su_t1980, speech_protection_boundary__harm_limited_reading, suppression_requirement, 1980, 0.5).
narrative_ontology:measurement_basis(spbl_harm_limited_su_t1980, observed).
narrative_ontology:measurement(spbl_harm_limited_su_t1995, speech_protection_boundary__harm_limited_reading, suppression_requirement, 1995, 0.55).
narrative_ontology:measurement_basis(spbl_harm_limited_su_t1995, observed).
narrative_ontology:measurement(spbl_harm_limited_su_t2010, speech_protection_boundary__harm_limited_reading, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement_basis(spbl_harm_limited_su_t2010, observed).
narrative_ontology:measurement(spbl_harm_limited_su_t2025, speech_protection_boundary__harm_limited_reading, suppression_requirement, 2025, 0.64).
narrative_ontology:measurement_basis(spbl_harm_limited_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__harm_limited_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, speech_protection_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__harm_limited_reading, speech_protection_boundary__balancing_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'speech protection' decomposes into three structurally distinct constraints corresponding to the three readings of the speech_protection_boundary kernel. This file is the harm_limited_reading (protected set narrowed by a categorical harm condition; state gatekeeping). The absolutist_reading confines the unprotected set to imminent lawless action; the balancing_reading determines protection case-by-case. Each file carries its own epsilon, beneficiaries, and victims; the upstream/downstream pressure between them runs through shared doctrinal infrastructure (this reading's harm categories supply the balancing reading's harm inputs, and absolutist jurisprudence supplies the resistance this regime meets). Edges here link the family for contamination-propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
