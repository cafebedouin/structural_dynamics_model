% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__balancing_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__balancing_reading, []).

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
 *   constraint_id: speech_protection_boundary__balancing_reading
 *   human_readable: Speech Protection Boundary — Case-by-Case Balancing Reading
 *   domain: Constitutional Law / Political Philosophy / Speech Regulation
 *
 * SUMMARY:
 *   This constraint models the balancing reading of the speech-protection
 *   kernel: courts determine, case by case, whether particular expression is
 *   protected by weighing First Amendment interests against other
 *   constitutional values (equality, dignity, public order) and demonstrated
 *   harms. Unlike the absolutist reading (protection near-absolute, harm
 *   exception limited to imminent lawless action) or the harm-limited reading
 *   (protection conditional on absence of significant dignitary/equality
 *   harm), this reading treats the boundary as inherently contextual and
 *   distributes gatekeeping discretion across the judiciary rather than
 *   fixing it in a categorical rule. This story authors ONLY the balancing
 *   reading as its own ε-invariant constraint; the sibling readings are
 *   separate constraints linked structurally, not blended into this one.
 *
 * KEY AGENTS:
 *   - reviewing_judiciary: administers the balancing test and retains ongoing discretion over where the boundary sits
 *   - targeted_harm_claimants: gain a doctrinal avenue unavailable under a categorical free-speech rule
 *   - marginal_speakers_with_unclear_categorization: bear unpredictability and chilling effects from ex-post categorization
 *   - coded_speech_users: occupy an especially unstable intermediate-scrutiny zone
 *   - well_resourced_institutional_litigants: shape the doctrine's evolution through repeated strategic litigation
 *   - absolutist_free_speech_advocates and dignitary_harm_focused_advocates: excluded from directly setting the weights despite bearing the doctrine's consequences
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__balancing_reading, 0.42).
domain_priors:suppression_score(speech_protection_boundary__balancing_reading, 0.48).
domain_priors:theater_ratio(speech_protection_boundary__balancing_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(speech_protection_boundary__balancing_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__balancing_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__balancing_reading, "Speech Protection Boundary — Case-by-Case Balancing Reading").
narrative_ontology:topic_domain(speech_protection_boundary__balancing_reading, "Constitutional Law / Political Philosophy / Speech Regulation").

domain_priors:requires_active_enforcement(speech_protection_boundary__balancing_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__balancing_reading, 'aac05780-1a2a-4683-8aed-e6df6c621358').
narrative_ontology:cs_kernel_codification('aac05780-1a2a-4683-8aed-e6df6c621358', distributed).
narrative_ontology:cs_authority_grounding('aac05780-1a2a-4683-8aed-e6df6c621358', lineage).
narrative_ontology:cs_interpretation_layer_present('aac05780-1a2a-4683-8aed-e6df6c621358').
narrative_ontology:cs_reading_relation('aac05780-1a2a-4683-8aed-e6df6c621358', speech_protection_boundary__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('aac05780-1a2a-4683-8aed-e6df6c621358', speech_protection_boundary__harm_limited_reading, coexists_with).
narrative_ontology:cs_axiom('aac05780-1a2a-4683-8aed-e6df6c621358', foundational, contextual_proportionality_governs_rights_conflicts).
narrative_ontology:cs_axiom_status(contextual_proportionality_governs_rights_conflicts, holdable).
narrative_ontology:cs_axiom_grounding('aac05780-1a2a-4683-8aed-e6df6c621358', contextual_proportionality_governs_rights_conflicts, instrumental).
narrative_ontology:cs_axiom('aac05780-1a2a-4683-8aed-e6df6c621358', secondary, no_single_constitutional_value_categorically_trumps).
narrative_ontology:cs_axiom_status(no_single_constitutional_value_categorically_trumps, holdable).
narrative_ontology:cs_axiom_grounding('aac05780-1a2a-4683-8aed-e6df6c621358', no_single_constitutional_value_categorically_trumps, conventional).
narrative_ontology:cs_reference_frame('aac05780-1a2a-4683-8aed-e6df6c621358', post_brandenburg_multifactor_scrutiny_regime).
narrative_ontology:cs_drift_state('aac05780-1a2a-4683-8aed-e6df6c621358', contemporary_platform_speech_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('aac05780-1a2a-4683-8aed-e6df6c621358', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__balancing_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, reviewing_judiciary).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, targeted_harm_claimants).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, constitutional_doctrine_flexibility_interests).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, marginal_speakers_with_unclear_categorization).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, coded_speech_users).
narrative_ontology:constraint_victim(speech_protection_boundary__balancing_reading, low_resource_litigants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__balancing_reading, well_resourced_institutional_litigants).
narrative_ontology:constraint_vindicates(speech_protection_boundary__balancing_reading, proportionality_in_rights_adjudication).
narrative_ontology:constraint_vindicates(speech_protection_boundary__balancing_reading, living_constitutionalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts apply multi-factor balancing tests to determine, case-by-case, whether particular speech is protected. This distributes enormous discretion to individual judges and panels, who weigh free-expression interests against claimed harms to dignity, equality, public order, or other constitutional values. The judiciary administers the boundary and can shift it opinion by opinion; no fixed rule binds future courts to the same weighting.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, reviewing_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Individuals or groups who can plead demonstrated harm from speech (harassment, discriminatory targeting, defamation-adjacent injury) gain a doctrinal avenue to seek restriction or redress that a categorical free-speech rule would foreclose. Their remedy depends entirely on persuading a court that the balance tips their way in this instance, which is uncertain but real leverage.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, targeted_harm_claimants, beneficiary,
    moderate, biographical, constrained, national).

% Speakers whose expression sits near a contested boundary (provocative political speech, artistic transgression, activist rhetoric) cannot know in advance whether a court will protect or punish them, because the standard is applied after the fact through discretionary weighing. They bear chilling effects and litigation risk that a bright-line rule would not impose, and have no reliable way to structure their conduct around a fixed rule.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, marginal_speakers_with_unclear_categorization, payer,
    powerless, immediate, trapped, local).

% Speakers using indirect, symbolic, or dog-whistle communication occupy a doctrinal gray zone that the balancing test treats as intermediate scrutiny territory — neither clearly protected core speech nor clearly unprotected incitement. Their liability turns on judicial inference about intent and effect, which is unpredictable and disproportionately burdens speakers without resources to litigate the inference.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, coded_speech_users, payer,
    powerless, immediate, trapped, national).

% Because the boundary is never settled by rule but must be relitigated in each new factual posture, defending a speech claim or pressing a harm claim requires sustained legal resources through appeal. Well-funded parties (media organizations, advocacy groups, institutions) can shape the case law that governs everyone; individuals without comparable resources absorb the uncertainty cost without comparable ability to move the doctrine.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, low_resource_litigants, payer,
    powerless, biographical, trapped, national).

% Media organizations, universities, large advocacy groups, and platforms have the resources to litigate test cases repeatedly, effectively shaping how the balancing factors get weighted over time. They can select favorable venues, fund strategic litigation, and absorb losses that would be existential for an individual, giving them outsized influence over where the boundary ultimately settles.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, well_resourced_institutional_litigants, beneficiary,
    powerful, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__balancing_reading, well_resourced_institutional_litigants, agenda_setter).

% Organizations and scholars committed to a near-categorical free-speech rule view the balancing approach as an unprincipled invitation to viewpoint-based suppression dressed as neutral weighing. Their preferred bright-line standard is a doctrinal option courts consistently decline to fully adopt; they participate in litigation and amicus practice but do not control the standard applied.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, absolutist_free_speech_advocates, excluded,
    organized, generational, constrained, national).

% Scholars and advocates who believe the balancing test still under-weights dignitary and equality harms relative to speech interests argue the current calibration protects too much harmful speech. They press for doctrinal shift toward the harm-limited reading but operate within the same discretionary structure they are critiquing, without power to fix the weighting themselves.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__balancing_reading, dignitary_harm_focused_advocates, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__balancing_reading, well_resourced_institutional_litigants).
narrative_ontology:fixing_cost_class(speech_protection_boundary__balancing_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for adjudicating genuinely hard cases where free-expression interests and other constitutional or social harms are both real and cannot be resolved by a simple categorical rule — allowing courts to accommodate new speech technologies, novel harms, and contextual nuance without constitutional amendment.
% TRANSFER_FUNCTION: Moves predictability and rule-of-law certainty away from ordinary speakers (who cannot know in advance how their speech will be categorized) toward the judiciary (which retains discretion) and toward whichever litigating party can sustain repeated case-by-case contests — typically well-resourced institutional actors and organized harm-claimant coalitions.
% ABSENT_VOICES: Ordinary individual speakers without litigation resources are rarely direct parties in the appellate cases that set the balancing factors; the doctrine is shaped largely by institutional litigants (media companies, universities, advocacy organizations) on both sides, while the diffuse population whose everyday speech is chilled by uncertainty has no seat in the room where the weights are set.
% DISAPPEARANCE_RATIONALE: If case-by-case balancing disappeared and were replaced overnight by a categorical rule (either absolutist or harm-limited), an enormous body of doctrine built around multi-factor tests, intermediate scrutiny, and context-sensitive harm assessment would become inapplicable; litigation strategy, legal practice around defamation/harassment/hate-speech claims, and judicial training would all have to reorganize around bright-line categories instead of discretionary weighing.
% FOUNDING_PROBLEM: Categorical free-speech rules proved unable to handle genuinely novel cases — new communication technologies, symbolic speech, group libel, targeted harassment campaigns — where treating speech as either fully protected or fully unprotected produced results that offended either free-expression values or victims' dignitary and safety interests. Balancing emerged to let courts accommodate both sets of values without waiting for constitutional amendment.
% FOUNDING_PROBLEM_CORROBORATION: Sitting judges and mainstream constitutional scholars attest the problem of genuinely hard cases remains live and requires ongoing judicial judgment. Independent critics on both flanks — free-speech absolutists and harm-focused reformers, neither of whom benefits from the balancing regime's persistence — attest instead that the discretionary structure has become an end in itself, generating unpredictability and forum-shopping incentives that a settled rule (in either direction) would eliminate; this cross-flank agreement from outside the judiciary's own institutional interest is notable corroboration that the founding problem's current handling, rather than its underlying existence, is what is contested.
narrative_ontology:disappearance_verdict(speech_protection_boundary__balancing_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__balancing_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__balancing_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_boundary__balancing_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__balancing_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__balancing_reading_tests).
:- end_tests(speech_protection_boundary__balancing_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 by interval end) and rising slowly — the balancing structure does not extract in the manner of a rent-seeking mechanism, but it does systematically transfer certainty away from low-resource individual speakers toward institutional repeat players who can litigate the standard into shape over time. Suppression (0.48) reflects real chilling effects on marginal and coded speech that a bright-line rule would not produce, though this is well below a genuinely coercive constraint. Theater ratio is modest (0.28) — most of the judicial weighing is substantively engaged with real constitutional tension, not performative; the small rising trend reflects growing use of multi-factor tests as a rhetorical device to reach predetermined outcomes in politically salient cases. accessibility_collapse is moderate-low (0.35): categorical alternatives (absolutist or harm-limited rules) remain fully articulable and are actively argued by organized advocates, so alternatives have not collapsed even though the balancing approach currently dominates practice. resistance is substantial (0.55), coming from both flanks who would replace balancing with a categorical rule.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's seat, this is principled adjudication accommodating irreducible complexity. From the marginal speaker's seat, the identical structure is an unpredictable exposure to liability determined after the fact by factors they cannot control in advance. From the well-resourced institutional litigant's seat, the same structure is a malleable doctrine they can help shape through sustained litigation investment. The engine computes these divergent seat classifications from the declared power/exit/scope data; the single claimed_type is my analytical judgment of the dominant structural character, not a reconciliation of these views.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary sits as agenda_setter with analytical exit — it administers rather than experiences the constraint from a beneficiary or victim position. Targeted harm claimants and well-resourced institutional litigants derive low d (beneficiary-leaning): the former gain a remedy avenue, the latter gain outsized influence over doctrinal evolution through repeat play. Marginal speakers, coded-speech users, and low-resource litigants derive high d (target-leaning): they bear the unpredictability cost of a standard that must be relitigated rather than looked up, with no exit — trapped exit options reflect that ordinary speech conduct cannot be restructured around an unstable rule. Excluded advocacy groups on both flanks have organized power but constrained exit: they can argue for doctrinal change but cannot unilaterally supply it.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (categorical rules failing on genuinely hard cases) is contested rather than resolved or clearly dead: hard cases plausibly still exist, but the R5 corroboration shows independent critics from BOTH ideological flanks — who gain nothing from the balancing regime's continuation — converging on the claim that the discretionary structure has outlived clean justification and now primarily generates unpredictability and forum-shopping. This convergence from outside the judiciary's own institutional interest is the strongest evidence available that the mandate has partially decoupled from its founding function, without being fully resolved into either dead or clearly still-live. Classifying this as tangled_rope (rather than snare) preserves the genuine coordination function — hard cases requiring contextual judgment plainly exist — while still registering the asymmetric extraction of certainty from powerless marginal speakers toward institutional repeat players, which a pure-coordination 'rope' label would erase.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    balancing_reading_kernel_disagreement_location,
    'Where exactly does the balancing reading''s structural claim diverge from its sibling readings, and is the divergence resolvable by evidence or only by prior commitment to a decision procedure?',
    'Compare case outcomes across jurisdictions/eras that have adopted more categorical (absolutist or harm-limited) rules versus balancing regimes for structurally similar speech disputes, tracking predictability, harm incidence, and chilling-effect measures.',
    'If categorical rules produce comparable harm-prevention and free-expression outcomes with substantially higher predictability, that would support reclassifying the balancing reading''s coordination claim as weaker than authored here (its indeterminacy would look more purely costly and less like irreducible complexity-management). If categorical rules produce worse outcomes on one axis or the other, it supports the balancing reading''s premise that irreducible contextual judgment is required.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(balancing_reading_kernel_disagreement_location, conceptual, 'Locates the balancing reading''s core structural disagreement with the absolutist and harm-limited readings: is contextual weighing solving a real problem categorical rules cannot, or is it importing costly discretion where a rule would do as well?').

omega_variable(
    sibling_reading_structural_delta,
    'What would adopting the absolutist_reading or harm_limited_reading change structurally about who benefits and who pays, relative to this balancing reading?',
    'Model the same stakeholder set (marginal speakers, coded-speech users, harm claimants, institutional litigants) under each reading''s decision rule and compare predicted victim/beneficiary reassignment.',
    'Under the absolutist_reading, targeted_harm_claimants would likely shift from beneficiary to payer (losing their balancing-based remedy avenue) while marginal_speakers_with_unclear_categorization would shift toward beneficiary (gaining predictability). Under the harm_limited_reading, the reverse shift would occur, with coded_speech_users potentially becoming more exposed rather than less. This confirms the three readings are structurally distinct constraints with different victim sets, not three measurements of one constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Documents how beneficiary/victim assignment would restructure under each sibling reading, confirming the readings are separate constraints per the ε-invariance principle.').

omega_variable(
    judicial_discretion_capture_risk,
    'Does the distributed gatekeeper role (individual judges applying multi-factor tests) create a systematic capture channel favoring whichever litigants can sustain repeat appellate practice, independent of the substantive merits of any given case?',
    'Track win rates and doctrinal citation influence of repeat institutional litigants versus first-time individual litigants across a sample of balancing-test speech cases over multiple decades.',
    'If repeat institutional litigants show disproportionate influence on how balancing factors get weighted going forward (beyond what case merits alone would predict), this strengthens the tangled_rope classification''s extraction component and would argue against treating the well_resourced_institutional_litigants'' beneficiary status as merely incidental.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_discretion_capture_risk, empirical, 'Whether the balancing regime systematically advantages repeat institutional players in shaping the very standard applied to everyone.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__balancing_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_boundary__balancing_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(spee_tr_t10, speech_protection_boundary__balancing_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(spee_tr_t20, speech_protection_boundary__balancing_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(spee_tr_t30, speech_protection_boundary__balancing_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement(spee_tr_t40, speech_protection_boundary__balancing_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement(spee_tr_t50, speech_protection_boundary__balancing_reading, theater_ratio, 50, 0.28).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_boundary__balancing_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(spee_be_t10, speech_protection_boundary__balancing_reading, base_extractiveness, 10, 0.31).
narrative_ontology:measurement(spee_be_t20, speech_protection_boundary__balancing_reading, base_extractiveness, 20, 0.35).
narrative_ontology:measurement(spee_be_t30, speech_protection_boundary__balancing_reading, base_extractiveness, 30, 0.38).
narrative_ontology:measurement(spee_be_t40, speech_protection_boundary__balancing_reading, base_extractiveness, 40, 0.4).
narrative_ontology:measurement(spee_be_t50, speech_protection_boundary__balancing_reading, base_extractiveness, 50, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_boundary__balancing_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(spee_su_t10, speech_protection_boundary__balancing_reading, suppression_requirement, 10, 0.36).
narrative_ontology:measurement(spee_su_t20, speech_protection_boundary__balancing_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(spee_su_t30, speech_protection_boundary__balancing_reading, suppression_requirement, 30, 0.43).
narrative_ontology:measurement(spee_su_t40, speech_protection_boundary__balancing_reading, suppression_requirement, 40, 0.46).
narrative_ontology:measurement(spee_su_t50, speech_protection_boundary__balancing_reading, suppression_requirement, 50, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__balancing_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_boundary__balancing_reading, 0.12).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, speech_protection_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__balancing_reading, speech_protection_boundary__harm_limited_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three siblings decomposing the natural-language concept 'where does First Amendment protection end.' Each sibling reading (absolutist, balancing, harm_limited) is authored as its own ε-invariant constraint with its own beneficiary/victim structure, since the readings produce structurally different classifications and different victim sets rather than different measurements of the same arrangement. All three are linked bidirectionally via affects_constraints; the balancing reading sits structurally between the other two, since it is the reading most likely to absorb doctrinal pressure exerted by advocates of either categorical alternative.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
