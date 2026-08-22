% ============================================================================
% CONSTRAINT STORY: sex_gender_category__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_gender_category__hybrid_reading, []).

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
 *   constraint_id: sex_gender_category__hybrid_reading
 *   human_readable: Sex/Gender Category Membership via Medical Transition (Hybrid Reading)
 *   domain: social/legal/medical
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested sex/gender
 *   category kernel. The hybrid reading defines category membership through a
 *   combination of immutable biology AND achieved social/medical transition
 *   status. Under this reading, trans women are conditionally admitted to the
 *   female category after completing specified medical milestones (hormone
 *   therapy duration, surgical procedures, psychological gatekeeping). This
 *   reading mediates between pure-biology readings (which exclude trans women
 *   categorically) and identity readings (which admit based on
 *   self-identification alone). The hybrid reading benefits medical
 *   institutions and institutional categorizers by creating a legible ruleset
 *   backed by medical authority; it extracts from trans people who cannot or
 *   will not medically transition (permanent exclusion) and from those
 *   mid-transition (delayed access, gatekeeping costs). The constraint is
 *   CLAIMED as tangled_rope (genuine coordination function + asymmetric
 *   extraction) and MEASURED as substantially extractive with high
 *   suppression — the engine will test whether the coordination function is
 *   genuine or cover-story.
 *
 * KEY AGENTS:
 *   - Medical gatekeepers (institutional, arbiter of medical transition sufficiency)
 *   - Trans women post-transition (moderate power, identity-locked, conditionally beneficiary)
 *   - Trans women pre-transition and non-medical trans people (powerless, identity-locked, victims)
 *   - Biology essentialists (organized, semi-beneficiary through biology preservation)
 *   - Institutional categorizers (institutional agenda-setter, beneficiary through legibility)
 *   - Gender-identity advocates (organized, excluded from authority structure)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__hybrid_reading, 0.68).
domain_priors:suppression_score(sex_gender_category__hybrid_reading, 0.72).
domain_priors:theater_ratio(sex_gender_category__hybrid_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__hybrid_reading, "Sex/Gender Category Membership via Medical Transition (Hybrid Reading)").
narrative_ontology:topic_domain(sex_gender_category__hybrid_reading, "social/legal/medical").

domain_priors:requires_active_enforcement(sex_gender_category__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__hybrid_reading, '99b113ca-b2a6-4cd9-bd74-fb327a75f739').
narrative_ontology:cs_kernel_codification('99b113ca-b2a6-4cd9-bd74-fb327a75f739', distributed).
narrative_ontology:cs_authority_grounding('99b113ca-b2a6-4cd9-bd74-fb327a75f739', extraction).
narrative_ontology:cs_interpretation_layer_present('99b113ca-b2a6-4cd9-bd74-fb327a75f739').
narrative_ontology:cs_reading_relation('99b113ca-b2a6-4cd9-bd74-fb327a75f739', sex_gender_category__biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('99b113ca-b2a6-4cd9-bd74-fb327a75f739', sex_gender_category__identity_reading, coexists_with).
narrative_ontology:cs_axiom('99b113ca-b2a6-4cd9-bd74-fb327a75f739', foundational, biology_epistemically_foundational_but_mutable).
narrative_ontology:cs_axiom_status(biology_epistemically_foundational_but_mutable, holdable).
narrative_ontology:cs_axiom_grounding('99b113ca-b2a6-4cd9-bd74-fb327a75f739', biology_epistemically_foundational_but_mutable, deontological).
narrative_ontology:cs_axiom('99b113ca-b2a6-4cd9-bd74-fb327a75f739', foundational, medical_transition_legitimate_reclassification_ground).
narrative_ontology:cs_axiom_status(medical_transition_legitimate_reclassification_ground, holdable).
narrative_ontology:cs_axiom_grounding('99b113ca-b2a6-4cd9-bd74-fb327a75f739', medical_transition_legitimate_reclassification_ground, instrumental).
narrative_ontology:cs_axiom('99b113ca-b2a6-4cd9-bd74-fb327a75f739', secondary, identity_alone_insufficient_institutional_verification).
narrative_ontology:cs_axiom_status(identity_alone_insufficient_institutional_verification, holdable).
narrative_ontology:cs_axiom_grounding('99b113ca-b2a6-4cd9-bd74-fb327a75f739', identity_alone_insufficient_institutional_verification, conventional).
narrative_ontology:cs_reference_frame('99b113ca-b2a6-4cd9-bd74-fb327a75f739', post_stonewall_medicalized_gatekeeping_era).
narrative_ontology:cs_drift_state('99b113ca-b2a6-4cd9-bd74-fb327a75f739', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('99b113ca-b2a6-4cd9-bd74-fb327a75f739', '2026-06-12T14:37:22Z').
narrative_ontology:cs_kernel_id(sex_gender_category__hybrid_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, medical_gatekeepers).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, biology_essentialists).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, institutional_categorizers).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, trans_women_pre_transition).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, non_medical_trans_people).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, trans_individuals_below_gatekeeping_threshold).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, trans_women_post_transition).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, non_trans_women).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, biology_essentialists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Psychiatrists, endocrinologists, and surgical specialists define what counts as sufficient medical transition (hormone duration, surgical completion, psychological readiness assessments) for category membership. They control access to transition-enabling care and produce the certification documents institutions recognize. Their authority is framed as clinical expertise; their gate-setting function defines who is 'real' enough for reclassification.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, medical_gatekeepers, agenda_setter,
    institutional, generational, arbitrage, national).

% Gain legal category reclassification and access to women's spaces/services after completing medical milestones. They benefit from the hybrid framework's conditional inclusion over pure biology readings, but only if they can navigate and sustain the medical-transition pathway. Their exit from the constraint is identity-fused (transitioning is constitutive of self-conception); remaining in place means continued misgendering.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, trans_women_post_transition, beneficiary,
    moderate, biographical, identity_locked, national).

% Bear the cost of being excluded from female category membership while unable to afford or access the medical pathway. They face legal misgendering, denial of facilities access, and institutional barriers — with no legitimate avenue (under this reading) to achieve reclassification without medical completion. Identity-locked: rejecting the identity would resolve the exclusion but contradicts self-conception.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, trans_women_pre_transition, payer,
    powerless, biographical, identity_locked, national).

% Trans people who do not transition medically (hormonal or surgical) are permanently excluded from category reclassification under this reading. They may reject medical transition on grounds of cost, health risk, absence of desire for medical intervention, or other reasons; the hybrid framework offers no path to category membership for them. Identity-locked by same mechanism as pre-transition group.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, non_medical_trans_people, payer,
    powerless, biographical, identity_locked, national).

% Gain institutional validation of the view that biological sex is category-foundational, even if medically modified. The hybrid reading preserves biology's epistemic weight by requiring medical proof of commitment to biological change, rather than accepting identity alone. They pay through the constraint by accepting the medical criterion as supplement to biology, but they extract through the institutional enforcement of biology-first framing.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, biology_essentialists, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__hybrid_reading, biology_essentialists, payer).

% Schools, workplaces, legal registries, and facilities administrators who implement category-based policies. The hybrid reading provides a clear legible rule: accept reclassification if medical transition is documented. This reduces ambiguity compared to identity-based readings and delays facilities disputes until gatekeeping thresholds are met. They benefit from having a ruleset backed by medical authority and reduce political friction through medicalization.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, institutional_categorizers, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__hybrid_reading, institutional_categorizers, beneficiary).

% Retain category-membership clarity under the hybrid reading: their female category is secured by immutable biology, and trans women are admitted only after substantial medical barrier-crossing. Under identity readings, their category becomes potentially contestable (identity-only inclusion). The hybrid reading offers them intermediate stability — shared category with trans women post-transition, but gated by medical requirement.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, non_trans_women, beneficiary,
    moderate, biographical, mobile, national).

% Would argue that medical transition should not be a prerequisite for category membership, and that identity-based inclusion is both ethically required and empirically sound. Their position is structurally excluded from the hybrid framework's authority structure — they are not seated as experts or decision-makers. They lack the institutional power to redefine the category absent legislative action.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, gender_identity_advocates, excluded,
    organized, biographical, constrained, national).

% Administrative and legal bodies that enforce non-discrimination law and interpret category-membership rules for institutional compliance. They observe the gating mechanism and assess whether medical-transition requirements constitute unlawful discrimination. They can declare the pathway illegal, mandate alternative access routes, or affirm it as a legitimate classification system.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, civil_rights_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sex_gender_category__hybrid_reading, medical_gatekeepers).
narrative_ontology:fixing_cost_class(sex_gender_category__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a legible, documentable category boundary that reconciles biological sex with social/medical recognition of transition, enabling institutions to implement facilities and services policies without requiring case-by-case identity determination. Reduces administrative ambiguity and political friction around contested categories.
% TRANSFER_FUNCTION: Moves institutional authority and access to category-reclassification from the trans person's own identity claim to the medical establishment. Authority over who counts as female is transferred from individual determination to licensed medical professionals and regulatory institutions. Moves access to women's spaces and legal recognition from identity-based (cheap, individual) to medical pathway-based (expensive, institutional).
% ABSENT_VOICES: Gender-identity advocates and trans people who reject medicalization are structurally excluded from the authority structure that defines the gating criteria. They can testify to regulatory bodies but do not set the framework. Non-trans women's views on category membership are heard through institutional channels but not directly represented in gatekeeping medical decisions.
% DISAPPEARANCE_RATIONALE: If the medical-transition requirement vanished overnight, institutions would face immediate reclassification decisions for trans individuals currently in pre-transition states. Some would shift to identity-based acceptance (aligning with identity_reading); others would revert to pure-biology gatekeeping (biology_reading). The legal category system would experience discontinuity until a new stable framework was established. Trans people's access to facilities, legal documents, and institutional recognition would be immediately contested.
% FOUNDING_PROBLEM: Early institutional responses to trans identity faced a binary: either deny trans women's womanhood entirely (pure biology reading) or accept subjective identity claims without institutional verification (pure identity reading). Both posed coordination problems — one excluded trans women categorically; the other created institutional vulnerability to disputes about who could credibly claim female status. The medical-transition requirement emerged as a compromise: it provided objective criteria (medical documentation) while acknowledging trans women's legitimate claims to reclassification.
% FOUNDING_PROBLEM_CORROBORATION: Medical institutions and institutional categorizers attest that gatekeeping solved coordination problems. Gender-identity advocates and trans people excluded by medical requirements attest that the founding problem was artificially constructed and that the 'solution' introduced worse problems (exclusion of non-medical trans people, medicalization of identity). Independent human rights analysis finds the founding problem partially real (earlier administrative chaos) and partially manufactured (treating trans identity as inherently less credible than cis claims). The reading's own authority structure (medical gatekeepers) benefits directly from framing the problem as solved by their expanded role.
narrative_ontology:disappearance_verdict(sex_gender_category__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sex_gender_category__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__hybrid_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sex_gender_category__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sex_gender_category__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness reaches 0.68 by interval end because the medical pathway is expensive (time, money, health risk, psychological burden), gatekeeping thresholds are set by institutional actors with incentives to maintain gating power, and the constraint permanently excludes trans people who reject medicalization. The trajectory rises from 0.55 to 0.68 as medical gatekeeping institutions consolidate authority: early in the interval, alternative (identity-based, bureaucratic) reclassification routes are still available in some jurisdictions; by interval end, medical pathway becomes institutionalized as the singular legitimate route, collapsing alternatives. Suppression is high (0.72) because the constraint actively excludes identity-based reclassification through institutional policy and law; non-medical paths are not merely unavailable, they are actively prohibited. Theater ratio (0.41) reflects that part of the medical gatekeeping function is legitimate risk-assessment and care-coordination, but an increasing share is administrative theater: proving 'seriousness,' demonstrating conformity to normative transition narratives, and maintaining gatekeeper authority itself. The separation of measurement grid is deliberate: every metric is authored at the same six time points across the interval to enable temporal analysis of drift.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (medical gatekeepers, institutional categorizers) experience this constraint as coordination machinery solving a real administrative problem; they measure their own function as legitimate professional gatekeeping. The victim seats (pre-transition trans women, non-medical trans people) experience it as sustained exclusion dependent on institutional power to suppress alternatives; they measure the same constraint as extractive coercion. The post-transition beneficiary seat experiences conditional inclusion but remains aware of the gatekeeping machinery: they cannot entirely unsee the arbitrariness of thresholds or the power differential. The biology-essentialist seat experiences partial satisfaction (biology remains epistemically foundational) but must accept medical modification of that biology as legitimate reclassification criterion — a compromise that extracts loyalty. The engine computes per-seat directionality from this structural asymmetry; the author documents it in plain language.
 *
 * DIRECTIONALITY LOGIC:
 *   Medical gatekeepers sit at d ≈ 0.1 (full beneficiaries: control authority, collect gatekeeping power, define legitimacy). Trans women post-transition sit at d ≈ 0.4 (mixed: gain category access but only via expensive, controlled pathway; identity-locked exit means they absorb the extraction cost rather than leave). Trans women pre-transition and non-medical trans people sit at d ≈ 0.9 (nearly full targets: permanently excluded, identity-locked so exit is not real option, bear full suppression cost). Biology essentialists sit at d ≈ 0.35 (moderate extraction: they extract through institutional reinforcement of biology-first framing, but they also pay through having to accept medical modification of that biology). Institutional categorizers sit at d ≈ 0.15 (full beneficiaries: get legible ruleset, reduced political friction, authority backed by medical expertise). The directionality spread across seats is what produces the per-seat type divergence: medical gatekeepers compute as rope-beneficiary (they coordinate and benefit); trans women pre-transition compute as snare-target (they are purely extracted from); post-transition trans women compute as tangled-rope mid-seat (they coordinated into a solution that extracts from them). This divergence is STRUCTURAL, not a measurement error.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (coordination chaos under identity-only vs. pure-biology readings) was real but is contested as SOLVED. The question is whether the medical-transition requirement was the only way to solve it, or whether it was one solution that benefited medical institutions and created new harms. The mandatrophy signal: the founding problem was institutional coordination (institutions need a clear ruleset). The hybrid reading 'solved' it by transferring authority to medicine. But medical authority depends on maintaining the gatekeeper role — there is extraction feeding back into problem-perpetuation. If institutions adopted an identity-based approach with simple documentary verification (not clinical gatekeeping), coordination would remain solved but extraction would collapse. The fact that institutions prefer the medical route suggests the founding problem was real but the chosen solution reflects extractive institutional interests, not pure coordination necessity. Tangled_rope captures this: genuine coordination function + asymmetric extraction. A snare reading would claim the coordination function is entirely cover-story; a rope reading would claim extraction is negligible. This reading falls between: coordination is real, extraction is substantial.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gatekeeping_threshold_arbitrariness,
    'Are the medical thresholds for transition sufficiency (hormone duration, surgical completion, psychological readiness assessments) calibrated to genuine clinical necessity, or do they reflect institutional gatekeeping incentives?',
    'Comparative analysis across jurisdictions with different thresholds, plus tracking of medical rationale changes over time. If thresholds tighten or loosen without clinical justification changes, gatekeeping incentives are operative.',
    'If thresholds are arbitrary, the constraint is snare-like: the medical-transition requirement is pure extraction dressed in clinical language. If genuinely clinical, the extraction remains high but the coordination function is substantive (tangled_rope). The boundary between clinical necessity and institutional gatekeeping is empirically discoverable but not yet definitively resolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_threshold_arbitrariness, empirical, 'Whether medical thresholds reflect clinical necessity or institutional extraction.').

omega_variable(
    identity_locked_internalization,
    'For trans people excluded under the hybrid reading, is the measured suppression structural (external barriers to medical access and category reclassification) or internalized (trans people have incorporated the medical-transition requirement as a legitimate standard for their own identity/category validity)?',
    'Post-remediation outcomes: if medical barriers are removed and trans people still refuse to self-identify as their authentic category (because they internalized the medical standard as legitimate proof requirement), suppression is partially internalized. If barrier removal leads to rapid self-identification, suppression was structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests; the target carries the suppression internally. If structural, the suppression is reversible by barrier removal. This affects whether reclassifying trans people mid-transition to the identity_reading would dissolve the constraint or require sustained institutional change.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_locked_internalization, empirical, 'Whether suppression is structural (external barriers) or internalized (target-held standards).').

omega_variable(
    biology_essentialism_necessity,
    'Is the preservation of biology as epistemically foundational (via the requirement for medical proof of biological change) structurally necessary for institutional category systems, or is it a contingent social choice?',
    'Thought experiment and comparative institutional analysis: if institutions adopted identity-only category reclassification, would coordination break down, or would institutional practices simply shift to accommodate new boundaries?',
    'If biology is institutionally necessary, the hybrid reading''s extraction is justified by coordination cost reduction. If biology is contingent, the hybrid reading''s preservation of biology-first framing is extractive ideology, benefiting biology essentialists and institutions that profit from biological gatekeeping (medicine, law, surveillance systems).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(biology_essentialism_necessity, conceptual, 'Whether biology-based categorization is structurally necessary or contingent.').

omega_variable(
    hybrid_vs_identity_reading_divergence,
    'What are the materialized differences between the hybrid reading''s medical-transition gatekeeping and the identity_reading''s self-identification gatekeeping? Where do trans people end up differently?',
    'Tracking of reclassification rates, facilities access, legal document changes, institutional compliance under each reading across comparable jurisdictions.',
    'This omega documents the reading-choice consequences: the hybrid reading excludes pre-transition trans people and non-medical trans people entirely; the identity_reading admits them immediately. The biology_reading excludes all trans women. This constraint story measures what the hybrid reading does; sibling readings measure what they do. The reading choice is not a framing difference — it is a difference in victims and benefits.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hybrid_vs_identity_reading_divergence, empirical, 'Materialized differences between readings in reclassification access and institutional friction.').

omega_variable(
    medical_authority_scope_creep,
    'Has the scope of medical gatekeeping expanded over the interval beyond its founding role (ensuring trans people''s informed consent and basic safety)? Are medical evaluations drifting toward certification of ''true'' gender identity rather than assessment of medical readiness?',
    'Content analysis of medical assessment documentation and gatekeeping rationales over time. If psychological evaluations increasingly assess ''authenticity'' of identity rather than medical safety, scope creep is occurring.',
    'Scope creep would indicate the constraint is drifting toward snare (pure extraction: medical authority is repurposed as identity validation machinery without clinical basis). Stable scope would support tangled_rope (genuine medical gatekeeping + extractive gate-setting).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_authority_scope_creep, empirical, 'Whether medical gatekeeping authority is scope-creeping toward identity certification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__hybrid_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__hybrid_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(sex__tr_t5, sex_gender_category__hybrid_reading, theater_ratio, 5, 0.32).
narrative_ontology:measurement(sex__tr_t10, sex_gender_category__hybrid_reading, theater_ratio, 10, 0.36).
narrative_ontology:measurement(sex__tr_t15, sex_gender_category__hybrid_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(sex__tr_t20, sex_gender_category__hybrid_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(sex__tr_t25, sex_gender_category__hybrid_reading, theater_ratio, 25, 0.41).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__hybrid_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(sex__be_t5, sex_gender_category__hybrid_reading, base_extractiveness, 5, 0.59).
narrative_ontology:measurement(sex__be_t10, sex_gender_category__hybrid_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(sex__be_t15, sex_gender_category__hybrid_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(sex__be_t20, sex_gender_category__hybrid_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement(sex__be_t25, sex_gender_category__hybrid_reading, base_extractiveness, 25, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__hybrid_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement(sex__su_t5, sex_gender_category__hybrid_reading, suppression_requirement, 5, 0.66).
narrative_ontology:measurement(sex__su_t10, sex_gender_category__hybrid_reading, suppression_requirement, 10, 0.69).
narrative_ontology:measurement(sex__su_t15, sex_gender_category__hybrid_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement(sex__su_t20, sex_gender_category__hybrid_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement(sex__su_t25, sex_gender_category__hybrid_reading, suppression_requirement, 25, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__hybrid_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sex_gender_category__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, sex_gender_category__biology_reading).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, sex_gender_category__identity_reading).

% DUAL FORMULATION NOTE:
% The sex/gender category kernel decomposes into three structurally distinct constraints, one per reading. The hybrid_reading (this file) defines membership via biology + medical transition. The biology_reading defines membership via immutable biology alone (higher exclusion of trans women, lower extraction from post-transition trans women). The identity_reading defines membership via self-identification alone (no medical gatekeeping, lower suppression, no medical-institution beneficiaries). These are not the same constraint measured three ways — they have different beneficiary/victim sets, different ε values, and different authority structures. They share a kernel (the contested category definition) but instantiate different structural claims. The reading_relations and axioms in cs_structure document the logical relationships. This story models the hybrid reading's ε at ~0.68 (substantial extraction via gatekeeping); sibling readings model different ε values reflecting different gate-setup costs and victim sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sex_gender_category__hybrid_reading, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
