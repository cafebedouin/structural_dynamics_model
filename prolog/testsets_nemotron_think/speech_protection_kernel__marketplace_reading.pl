% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__marketplace_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__marketplace_reading, []).

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
 *   constraint_id: speech_protection_kernel__marketplace_reading
 *   human_readable: Marketplace of Ideas Speech Protection
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   The marketplace reading (Holmes/Brandeis Abrams dissent through mid-20th
 *   century jurisprudence) justifies strong speech protection as an epistemic
 *   engine: truth discovers itself through open competition, and the remedy
 *   for false/harmful speech is more speech, not restriction. This reading
 *   became the dominant First Amendment framework. But the constraint it
 *   supports — near-absolute protection against content-based regulation —
 *   operates differently across seats. For majority speakers and platform
 *   owners, it functions as genuine coordination (Rope). For targeted
 *   minorities and misinformation victims, it extracts harm without remedy
 *   (Snare). The engine computes this seat divergence from the structural
 *   data. The claimed_type (tangled_rope) reflects my analytical judgment
 *   that the constraint has BOTH a real coordination function (truth
 *   discovery sometimes works) AND asymmetric extraction (harm falls on the
 *   voiceless); the reading's own self-presentation is 'rope'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__marketplace_reading, 0.45).
domain_priors:suppression_score(speech_protection_kernel__marketplace_reading, 0.3).
domain_priors:theater_ratio(speech_protection_kernel__marketplace_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__marketplace_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__marketplace_reading, "Marketplace of Ideas Speech Protection").
narrative_ontology:topic_domain(speech_protection_kernel__marketplace_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__marketplace_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__marketplace_reading, '48ba8675-7237-4e1c-8d62-76c5dc86ff45').
narrative_ontology:cs_kernel_codification('48ba8675-7237-4e1c-8d62-76c5dc86ff45', fixed_text).
narrative_ontology:cs_authority_grounding('48ba8675-7237-4e1c-8d62-76c5dc86ff45', lineage).
narrative_ontology:cs_interpretation_layer_present('48ba8675-7237-4e1c-8d62-76c5dc86ff45').
narrative_ontology:cs_reading_relation('48ba8675-7237-4e1c-8d62-76c5dc86ff45', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('48ba8675-7237-4e1c-8d62-76c5dc86ff45', speech_protection_kernel__harm_threshold_reading, influences).
narrative_ontology:cs_reading_relation('48ba8675-7237-4e1c-8d62-76c5dc86ff45', speech_protection_kernel__dignity_reading, influences).
narrative_ontology:cs_reading_relation('48ba8675-7237-4e1c-8d62-76c5dc86ff45', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('48ba8675-7237-4e1c-8d62-76c5dc86ff45', foundational, truth_emerges_from_open_contest).
narrative_ontology:cs_axiom_status(truth_emerges_from_open_contest, holdable).
narrative_ontology:cs_axiom_grounding('48ba8675-7237-4e1c-8d62-76c5dc86ff45', truth_emerges_from_open_contest, empirically_contingent).
narrative_ontology:cs_axiom('48ba8675-7237-4e1c-8d62-76c5dc86ff45', foundational, content_based_restrictions_distort_truth_discovery).
narrative_ontology:cs_axiom_status(content_based_restrictions_distort_truth_discovery, holdable).
narrative_ontology:cs_axiom_grounding('48ba8675-7237-4e1c-8d62-76c5dc86ff45', content_based_restrictions_distort_truth_discovery, deontological).
narrative_ontology:cs_reference_frame('48ba8675-7237-4e1c-8d62-76c5dc86ff45', classical_liberal_epistemic_humility).
narrative_ontology:cs_drift_state('48ba8675-7237-4e1c-8d62-76c5dc86ff45', platform_amplification_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('48ba8675-7237-4e1c-8d62-76c5dc86ff45', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__marketplace_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, majority_speakers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, platform_owners).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, institutional_media).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, dominant_ideological_groups).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, targeted_minority_groups).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, structurally_silenced_speakers).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, misinformation_victims).
narrative_ontology:constraint_vindicates(speech_protection_kernel__marketplace_reading, epistemic_humility_principle).
narrative_ontology:constraint_vindicates(speech_protection_kernel__marketplace_reading, content_neutrality_doctrine).
narrative_ontology:constraint_vindicates(speech_protection_kernel__marketplace_reading, counterspeech_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enjoy near-unrestricted speech access through mainstream platforms and institutions; their speech enters the marketplace with amplification and rarely faces counterspeech they cannot match. Benefit from the constraint's protection without bearing its harms.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, majority_speakers, beneficiary,
    powerful, biographical, mobile, national).

% Control the infrastructure of the modern marketplace (algorithms, moderation policies, reach distribution). Invoke First Amendment protections against regulation while privately governing speech at scale. Collect economic rents from the attention economy the constraint enables.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, platform_owners, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__marketplace_reading, platform_owners, beneficiary).

% Legacy press institutions that shaped the marketplace reading's mid-century jurisprudence. Retain privileged access to audiences and legal resources (libel defense, newsgathering protections). Benefit from the constraint's shield against government interference.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, institutional_media, beneficiary,
    organized, biographical, mobile, national).

% Bear the concentrated harm of hate speech, harassment, and identity-based vilification protected by the constraint. The 'more speech' remedy fails because they lack equal platform access, face retaliation for counterspeech, and suffer dignitary harm that speech cannot undo. Cannot exit the constraint's effects.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, targeted_minority_groups, payer,
    powerless, generational, trapped, national).

% Speakers marginalized by economic, social, or platform barriers who cannot effectively enter the marketplace. The constraint protects their right to speak in theory but the marketplace's entry costs (audience, safety, amplification) exclude them in practice. Exit means silence.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, structurally_silenced_speakers, payer,
    moderate, biographical, constrained, national).

% Individuals and communities harmed by viral falsehoods (medical misinformation, election lies, defamation) that spread faster than correction. The constraint's 'counterspeech' remedy is empirically inadequate against algorithmic amplification. They bear the cost of the marketplace's epistemic pollution.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, misinformation_victims, payer,
    powerless, immediate, trapped, global).

% Adjudicate the constraint's boundaries through First Amendment doctrine. Their interpretation layer (strict scrutiny, content neutrality, public forum doctrine) absorbs drift and determines which speech gets marketplace access. Not a party to the marketplace but its institutional gatekeeper.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, courts, agenda_setter,
    institutional, generational, analytical, national).

% Analyze the constraint's epistemic claims and distributive effects from outside the dispute. Provide the empirical and normative assessment the marketplace reading itself cannot generate (e.g., Sunstein on echo chambers, Waldron on dignity harm, Benkler on network propaganda).
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, democratic_theorists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables open competition of ideas so that truth emerges from public testing rather than authoritative declaration; solves the epistemic problem of how a society discovers truth without a designated truth-authority.
% TRANSFER_FUNCTION: Moves the cost of harmful/false speech from speakers (who face no liability) to targeted groups and the public epistemics (who bear the harm of misinformation, hate speech, and polluted information environments); moves the benefit of unrestricted speech to those with platform access and amplification.
% ABSENT_VOICES: Future generations who inherit a degraded information environment; non-citizen residents subject to domestic speech harms; the structurally silenced who never enter the marketplace because the entry barriers (platform access, audience, safety) are too high.
% DISAPPEARANCE_RATIONALE: Without the constraint, content-based restrictions would proliferate; the epistemic benefit of open contest would be lost but so would the unchecked harms; a new equilibrium of regulated speech would emerge — likely resembling European dignity-based or harm-threshold regimes.
% FOUNDING_PROBLEM: The founding problem was authoritarian control of truth — sovereigns and churches suppressing dissenting ideas, preventing scientific and moral progress by declaring orthodoxy. The marketplace reading was built to solve: how does a society discover truth without a censor?
% FOUNDING_PROBLEM_CORROBORATION: Historical record of Sedition Act enforcement, scientific suppression (Galileo), religious censorship corroborates the founding problem. But contemporary scholars (Sunstein, Waldron, Matsuda, Benkler) attest the problem has mutated: today's threat is not sovereign censorship but platform-amplified harm and epistemic pollution — a shift the marketplace reading does not address.
narrative_ontology:disappearance_verdict(speech_protection_kernel__marketplace_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__marketplace_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__marketplace_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_kernel__marketplace_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__marketplace_reading, 0.45, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__marketplace_reading_tests).
:- end_tests(speech_protection_kernel__marketplace_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) reflects that the constraint enables both truth discovery (coordination value) and unchecked harm to vulnerable groups (extraction). The rise from 0.15 to 0.45 tracks the mutation from sovereign censorship (where the constraint was protective) to platform amplification (where it enables harm at scale). Suppression (0.30) is moderate: the constraint suppresses government restriction, not speech itself, but its doctrinal rigidity (strict scrutiny) blocks harm-prevention laws. Theater ratio (0.25) captures performative invocation of 'marketplace of ideas' to block platform regulation, dignity protections, and misinformation responses where the coordination function is known to fail. Accessibility collapse (0.40): harm-threshold and dignity readings persist as live alternatives but are doctrinally marginalized. Resistance (0.60): significant scholarly, judicial, and legislative pushback against marketplace absolutism.
 *
 * PERSPECTIVAL GAP:
 *   From the majority_speaker seat, the constraint is a Rope: it coordinates truth discovery, the harms are diffuse and the remedy works. From the targeted_minority_group seat, it is a Snare: the coordination story is cover for protecting the powerful's right to harm the powerless. The engine computes this divergence from the structural data — the authored claim does not adjudicate it. Platform owners occupy a dual seat: agenda_setter (they govern the marketplace) and beneficiary (they profit from it), creating a self-reinforcing loop where the constraint's administrator captures its gains.
 *
 * DIRECTIONALITY LOGIC:
 *   Majority speakers, platform owners, and institutional media are structural beneficiaries (d near 0.0): they collect the constraint's coordination benefits and avoid its harms. Targeted minority groups, structurally silenced speakers, and misinformation victims are structural targets (d near 1.0): they bear concentrated harm with no effective exit or counterspeech remedy. Courts sit at d ≈ 0.5 (institutional gatekeepers who administer the constraint but also bear legitimacy costs). Democratic theorists are analytical (d = 0.5 by definition). The 'more speech' remedy assumes symmetric voice; where voice is asymmetric, the constraint's directionality flips from coordination to extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (authoritarian truth-control) is real but mutated. The marketplace reading persists as doctrine because its interpretive layer (First Amendment jurisprudence) absorbs drift — but the drift is axiom_overriding: the empirical premise that 'more speech counters false speech' is substantially challenged by misinformation research, network propaganda studies, and the reality of algorithmic amplification. The constraint has not adapted; its mandate has atrophied relative to the new epistemic environment. Mandatrophy is unresolved: the arrangement persists while its founding justification erodes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    marketplace_equality_precondition,
    'Is the marketplace of ideas structurally dependent on rough equality of voice, access, and safety to function as a truth-discovery mechanism?',
    'Empirical study of truth-discovery outcomes in environments with varying voice inequality (e.g., comparative media systems, platform experiments with amplification controls). If truth-discovery degrades sharply above an inequality threshold, the coordination function is conditional on equality the constraint does not guarantee.',
    'If equality is a precondition, the marketplace reading''s coordination claim holds only where the constraint''s own operation (protecting unequal power) doesn''t undermine it — a structural self-defeat. The constraint would be tangled_rope at best, snare where inequality is high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marketplace_equality_precondition, empirical, 'Whether the coordination function requires equality the constraint itself prevents regulating.').

omega_variable(
    more_speech_remedy_efficacy,
    'Does ''more speech'' actually counter false/harmful speech in conditions of asymmetric power, algorithmic amplification, and identity-targeted vilification?',
    'Longitudinal studies of counterspeech effectiveness across harm types (hate speech, medical misinformation, election lies, defamation) and speaker power differentials. Natural experiments from platform interventions (downranking, labeling, removal).',
    'If the remedy systematically fails for structural reasons (not contingent enforcement), the constraint''s coordination function is fictitious for the harms it enables. The extraction becomes the dominant structure — reclassification toward snare for payer seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(more_speech_remedy_efficacy, empirical, 'Whether the constraint''s claimed remedy functions where its protection creates the harm.').

omega_variable(
    kernel_reading_location,
    'This constraint is one reading (marketplace_reading) of the speech_protection_kernel. The sibling readings are absolutist_reading, harm_threshold_reading, dignity_reading, democratic_participation_reading. Where exactly is the structural disagreement located?',
    'Map each reading''s axioms and reference frames to identify which structural element they contest: the justification ground (epistemic vs. autonomy vs. dignity vs. democratic vs. harm), the empirical premise (counterspeech works vs. fails), the scope of protection (categorical vs. conditional), or the authority grounding (lineage vs. expertise vs. practice).',
    'Clarifies whether the kernel contains one constraint with interpretive variance or multiple structurally distinct constraints (per ε-invariance). If ε differs across readings, they are separate constraints linked by affects_constraints, not one constraint with observer-relative classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_location, conceptual, 'Structural location of disagreement within the speech_protection_kernel across its five declared readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__marketplace_reading, 0, 105).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__marketplace_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(spee_tr_t25, speech_protection_kernel__marketplace_reading, theater_ratio, 25, 0.08).
narrative_ontology:measurement(spee_tr_t50, speech_protection_kernel__marketplace_reading, theater_ratio, 50, 0.12).
narrative_ontology:measurement(spee_tr_t75, speech_protection_kernel__marketplace_reading, theater_ratio, 75, 0.18).
narrative_ontology:measurement(spee_tr_t90, speech_protection_kernel__marketplace_reading, theater_ratio, 90, 0.22).
narrative_ontology:measurement(spee_tr_t105, speech_protection_kernel__marketplace_reading, theater_ratio, 105, 0.25).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__marketplace_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(spee_be_t25, speech_protection_kernel__marketplace_reading, base_extractiveness, 25, 0.2).
narrative_ontology:measurement(spee_be_t50, speech_protection_kernel__marketplace_reading, base_extractiveness, 50, 0.25).
narrative_ontology:measurement(spee_be_t75, speech_protection_kernel__marketplace_reading, base_extractiveness, 75, 0.35).
narrative_ontology:measurement(spee_be_t90, speech_protection_kernel__marketplace_reading, base_extractiveness, 90, 0.42).
narrative_ontology:measurement(spee_be_t105, speech_protection_kernel__marketplace_reading, base_extractiveness, 105, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__marketplace_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(spee_su_t25, speech_protection_kernel__marketplace_reading, suppression_requirement, 25, 0.15).
narrative_ontology:measurement(spee_su_t50, speech_protection_kernel__marketplace_reading, suppression_requirement, 50, 0.2).
narrative_ontology:measurement(spee_su_t75, speech_protection_kernel__marketplace_reading, suppression_requirement, 75, 0.25).
narrative_ontology:measurement(spee_su_t90, speech_protection_kernel__marketplace_reading, suppression_requirement, 90, 0.28).
narrative_ontology:measurement(spee_su_t105, speech_protection_kernel__marketplace_reading, suppression_requirement, 105, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__marketplace_reading, information_standard).
narrative_ontology:boltzmann_floor_override(speech_protection_kernel__marketplace_reading, 0.02).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% The speech_protection_kernel decomposes into five constraint stories (one per reading) because each reading instantiates a different constraint with different ε, different beneficiary/victim structure, and different type. The marketplace reading's ε (0.45) differs from the absolutist reading's (lower, ~0.15, because it denies harm exists) and the dignity reading's (higher, ~0.65, because it centers subordination harm). Per ε-invariance principle: these are separate constraints linked by affects_constraints, not one constraint measured differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_kernel__marketplace_reading, institutional, 0.15).
constraint_indexing:directionality_override(speech_protection_kernel__marketplace_reading, powerful, 0.2).
constraint_indexing:directionality_override(speech_protection_kernel__marketplace_reading, organized, 0.25).
constraint_indexing:directionality_override(speech_protection_kernel__marketplace_reading, moderate, 0.7).
constraint_indexing:directionality_override(speech_protection_kernel__marketplace_reading, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
