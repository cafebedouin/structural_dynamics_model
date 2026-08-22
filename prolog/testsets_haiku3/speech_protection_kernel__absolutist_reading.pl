% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__absolutist_reading, []).

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
 *   constraint_id: speech_protection_kernel__absolutist_reading
 *   human_readable: Absolutist Speech Protection (Near-Categorical Protection Boundary)
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint instantiates the absolutist reading of the
 *   speech-protection kernel: the doctrine that expression receives
 *   near-categorical protection; listener harm is structurally ineligible as
 *   grounds for restriction; speaker autonomy is the constitutive good; the
 *   state may regulate only narrow categorical exclusions (true threats,
 *   imminent incitement, obscenity, child sexual abuse material). The reading
 *   is contested by four sibling readings: democratic-participation
 *   (protection strongest for political speech necessary for
 *   self-governance), dignity (protection conditional on not functioning as
 *   structural subordination), harm-threshold (protection conditional on
 *   absence of demonstrable harm), and marketplace (protection serves
 *   truth-discovery; false speech countered by more speech). This story
 *   authors the absolutist reading as a clean constraint — one ε, one
 *   beneficiary/victim structure, one classification — and documents the
 *   competing readings through omega variables and cs_structure linkages, not
 *   through internal hedging. The claim (rope) reflects the absolutist
 *   framing's self-description: speaker autonomy creates genuine coordination
 *   (a stable boundary that cannot move with listener preference). The
 *   authored metrics describe a constraint whose persistence requires active
 *   judicial enforcement against harm-threshold pressure and whose extractive
 *   force on harm-claiming groups has grown over the interval as social
 *   awareness of speech's subordinating potential has risen.
 *
 * KEY AGENTS:
 *   - Speakers expressing heterodox, unpopular, or dissenting views — beneficiaries of categorical protection even when their speech wounds listeners
 *   - Harm-claiming groups (communities targeted by slurs, subordinating speech, scientific denialism) — structural payers who lose the harm-complaint lever
 *   - State regulatory apparatus (legislatures, administrators, some courts) — constrained to narrow categorical exclusions; cannot respond to democratic pressure for harm-based restrictions
 *   - Judiciary (First Amendment courts) — agenda-setter that applies the categorical-exclusions test and rejects harm-based restrictions
 *   - Private institutional gatekeepers (platforms, publishers) — nominally free to moderate but face reputational pressure from absolutist framing that treats editorial removal as 'censorship'
 *   - Democratic legislative majorities — excluded from harm-based restriction authority even when democratically enacted
 *   - Comparative constitutional observers — analytical seat assessing empirical correlates and constitutional friction points
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__absolutist_reading, 0.68).
domain_priors:suppression_score(speech_protection_kernel__absolutist_reading, 0.55).
domain_priors:theater_ratio(speech_protection_kernel__absolutist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__absolutist_reading, rope).
narrative_ontology:human_readable(speech_protection_kernel__absolutist_reading, "Absolutist Speech Protection (Near-Categorical Protection Boundary)").
narrative_ontology:topic_domain(speech_protection_kernel__absolutist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_kernel__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__absolutist_reading, '6e464899-e9f4-4f29-a67f-2c4b1de87757').
narrative_ontology:cs_kernel_codification('6e464899-e9f4-4f29-a67f-2c4b1de87757', formalized).
narrative_ontology:cs_authority_grounding('6e464899-e9f4-4f29-a67f-2c4b1de87757', lineage).
narrative_ontology:cs_interpretation_layer_present('6e464899-e9f4-4f29-a67f-2c4b1de87757').
narrative_ontology:cs_reading_relation('6e464899-e9f4-4f29-a67f-2c4b1de87757', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_reading_relation('6e464899-e9f4-4f29-a67f-2c4b1de87757', speech_protection_kernel__dignity_reading, coexists_with).
narrative_ontology:cs_reading_relation('6e464899-e9f4-4f29-a67f-2c4b1de87757', speech_protection_kernel__harm_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('6e464899-e9f4-4f29-a67f-2c4b1de87757', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_axiom('6e464899-e9f4-4f29-a67f-2c4b1de87757', foundational, speaker_autonomy_constitutive).
narrative_ontology:cs_axiom_status(speaker_autonomy_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('6e464899-e9f4-4f29-a67f-2c4b1de87757', speaker_autonomy_constitutive, deontological).
narrative_ontology:cs_axiom('6e464899-e9f4-4f29-a67f-2c4b1de87757', foundational, listener_harm_ineligible_ground).
narrative_ontology:cs_axiom_status(listener_harm_ineligible_ground, holdable).
narrative_ontology:cs_axiom_grounding('6e464899-e9f4-4f29-a67f-2c4b1de87757', listener_harm_ineligible_ground, deontological).
narrative_ontology:cs_reference_frame('6e464899-e9f4-4f29-a67f-2c4b1de87757', speaker_autonomy_categorical_boundary).
narrative_ontology:cs_drift_state('6e464899-e9f4-4f29-a67f-2c4b1de87757', contemporary_subordination_visibility_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('6e464899-e9f4-4f29-a67f-2c4b1de87757', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__absolutist_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, speakers_heterodox_unpopular).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, speakers_political_dissent).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, institutional_gatekeepers_private).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, harm_claiming_groups).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, state_regulatory_apparatus).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, institutional_gatekeepers_private).
narrative_ontology:constraint_vindicates(speech_protection_kernel__absolutist_reading, first_amendment_absolutism).
narrative_ontology:constraint_vindicates(speech_protection_kernel__absolutist_reading, speaker_autonomy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Speakers whose utterances are culturally disfavored, scientifically contested, or socially stigmatized. Under absolutist protection, they retain the right to speak regardless of listener discomfort, offense, or claimed harm. Their protection flows from the principle that speaker autonomy is categorical, not conditional on audience approval or harm thresholds. They bear no burden of proving utility or truth.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, speakers_heterodox_unpopular, beneficiary,
    powerless, biographical, constrained, national).

% Speakers whose political expression challenges state or institutional authority. Absolutist protection insulates them from harm-based or security-based censorship claims. Their utterances are protected even when governments claim they incite violence, undermine stability, or cause reputational damage to officials.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, speakers_political_dissent, beneficiary,
    powerless, biographical, constrained, national).

% Communities claiming harm from speech — that targeted utterances cause psychological distress, reinforce subordination, or create hostile environments. Under absolutist protection, their harm claims are structurally ineligible as grounds for restriction. They bear the burden of accepting speech that wounds them, with no recourse to legal protection grounded in listener harm alone.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, harm_claiming_groups, payer,
    organized, biographical, constrained, national).

% Government and institutional actors that would regulate speech on harm, security, dignity, or democratic-participation grounds. Absolutist protection strips these actors of the harm-claim lever: they retain only narrow categorical exclusions (true threats, incitement to imminent lawless action, obscenity, child exploitation). Their scope to regulate in response to social pressure is substantially constrained.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, state_regulatory_apparatus, payer,
    institutional, generational, trapped, national).

% Private platforms, publishers, and media entities. Absolutist doctrine does not constrain their editorial choices (they are not state actors), but it creates reputational and market pressure when they moderate based on harm claims — the absolutist frame treats their removal of speech as censorship, even though it is private action. They sit between legal freedom and social condemnation.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, institutional_gatekeepers_private, payer,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__absolutist_reading, institutional_gatekeepers_private, beneficiary).

% Courts tasked with interpreting and enforcing constitutional speech protection. Under absolutist doctrine, they operate a narrow-categorical-exclusions test: speech is presumptively protected unless it falls within the small set of regulable categories. They must reject harm, offense, and dignity-based restrictions even when the harm is real and the restrictor is democratically authorized.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, judiciary_first_amendment, agenda_setter,
    institutional, generational, analytical, national).

% Legislatures reflecting democratic majorities that wish to restrict speech on harm or social-stability grounds. Absolutist protection removes harm-based restrictions from their authority even when democratically enacted. They are excluded from the conversation the absolutist constraint governs — they would argue harm thresholds reflect democratic will, but absolutism structurally bars that argument.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, legislative_democratic_majority, excluded,
    institutional, generational, constrained, national).

% Constitutional theorists and comparative legal scholars observing the absolutist constraint's operation across different democratic systems and competing constitutional cultures. They take comparative testimony, analyze empirical correlates of absolutism, and identify where absolutist doctrine produces friction with competing constitutional commitments (dignity, equality, democratic participation).
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, observer_comparative_constitutional, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__absolutist_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_protection_kernel__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single, stable protection boundary for all speakers regardless of listener impact: speaker autonomy is the constitutional good; listener harm cannot be the measure of permissible speech.
% TRANSFER_FUNCTION: Moves the burden of harm acceptance from speakers to listeners — listeners must tolerate, ignore, or counter-speak rather than invoke state authority to silence speakers. Speakers retain utterance rights even when listeners are wounded; listeners lose the harm-complaint lever in the legal system.
% ABSENT_VOICES: Harm-claiming groups and democratic majorities who would restrict on harm grounds are structurally excluded — the absolutist frame does not admit their harm claims as part of the legitimate conversation about speech boundaries. They would argue that speaker autonomy cannot be categorical when speech functions as subordination or systematic silencing of other voices, but absolutism rules that argument out of order.
% DISAPPEARANCE_RATIONALE: If absolutist protection vanished and harm-based restrictions became legitimate, the legal and social architecture of expression would reorganize: legislatures would enact speech codes; platforms would moderate more aggressively; the burden of speaking unpopular truths would rise sharply; dissident speech would face new legal jeopardy; the speaker-listener asymmetry would invert in favor of organized listener-groups with harm claims.
% FOUNDING_PROBLEM: Governments exploit censorship authority to suppress dissent and heterodoxy, presenting suppression as protection of public order or social dignity. Speaker autonomy requires a protection boundary that cannot be moved by harm claims, lest state and institutional actors use harm-language to justify suppressing speakers they disfavor.
% FOUNDING_PROBLEM_CORROBORATION: Absolutist scholars and civil-liberties advocates attest that suppression-via-harm-claims remains a live threat in many jurisdictions (authoritarian governments, illiberal democracies). Harm-threshold and dignity scholars contest whether the founding problem is accurately described: they argue the problem is not that harm-grounds are used to suppress dissent, but that absolutism protects speech that FUNCTIONS as suppression of other voices' ability to participate. Independent comparative-constitutional research documents variation in how speech codes are deployed (some protect dissent, some suppress it), but does not resolve the empirical causal story behind the absolutist concern.
narrative_ontology:disappearance_verdict(speech_protection_kernel__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_kernel__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__absolutist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68 at interval end) because the constraint imposes a stable, asymmetric burden on harm-claiming groups — they must accept speech that wounds them with no legal recourse grounded in harm. The extraction has grown over the interval as social consciousness of speech-as-harm has risen (0.45 → 0.68) and pressure from harm-threshold readings has intensified. Suppression is moderate (0.55) because the constraint's persistence depends on active judicial enforcement of the categorical-exclusions boundary — courts must continually reject harm-threshold arguments and legislative attempts to broaden regulable categories. The suppression requirement has stabilized (0.42 → 0.55, then flat) because the boundary is now entrenched in constitutional doctrine and judicial practice; further enforcement gains are marginal. Theater is low-growing (0.08 → 0.22, then flat) because absolutist protection has increasingly rhetorical content — enforcement of narrow categorical exclusions (true threats, obscenity) consumes less judicial energy than rhetorical defense of absolutism itself against harm-threshold challenges. At t=75, theater has plateaued because the absolutist reading's institutional energy is now spent on maintaining the boundary rather than expanding it. The measurement series tracks a constraint whose functional extractiveness has accumulated as secondary harms (subordination effects, epistemic damage to targeted groups) have become visible to social actors, while the categorical boundary remains enforced.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary and payer seats should compute sharply different types from their structural positions. From a speaker's chair, the constraint is genuine coordination: a stable boundary protects autonomy and prevents suppression-via-harm-claims. From a harm-claiming group's chair, the same structure operates as enforced extraction: they must absorb wounds while their complaints are structurally ineligible. The judiciary's seat computes as near-beneficiary (high clarity, low discretion burden, protected from political pressure); the legislature's seat as pure target (authority stripped, will ignored, constrained by judicial enforcement). The engine computes these divergences from the beneficiary/victim declarations and power/exit data — no perspective override is needed. The perspectival gap is the measurement the constraint corpus exists to take: a reading whose beneficiary and payer seats would describe the same constraint as legitimacy-supporting or legitimacy-corroding is a signal of deep structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary seats: speakers expressing heterodox/dissenting views sit at d ≈ 0.1–0.2 (they collect protection; exit options improve dramatically under absolutist doctrine). Payer seats: harm-claiming groups sit at d ≈ 0.8–0.9 (they bear the asymmetric burden; their exit is to internalize the harm or organize counter-speech, both constrained). The state regulatory apparatus sits at d ≈ 0.75 (its authority over harm-based restrictions is stripped). Private gatekeepers sit at d ≈ 0.55–0.60 (they are free to moderate but face reputational cost from the absolutist frame). The judiciary sits at d ≈ 0.0–0.1 (it is the beneficiary of clarity and reduced discretion; the categorical-exclusions test removes difficult line-drawing). Democratic majorities sit at d ≈ 0.85 (they are excluded from authority and must accept outcomes they did not choose). Directionality reflects the asymmetry: absolutism protects speakers and constrains harm-claimers, not the reverse. No overrides are needed because the structural data (speaker beneficiary, harm-claiming payer, narrow exclusion vector, excluded legislature) produces the right directionality naturally.
 *
 * MANDATROPHY ANALYSIS:
 *   The absolutist reading faces a live mandatrophy question: was it built to solve the founding problem (suppression-via-harm-claims) and does that problem persist? The founding_problem_status is 'contested' because harm-threshold and dignity readings argue the founding problem has shifted — the contemporary problem is not state suppression of dissent via harm-claims, but speech-as-subordination (slurs, scientific denialism, conspiracy theories) that functions to silence other voices' participation. Under that reframing, absolutism now ENABLES the problem (suppression of targeted groups via unaccountable speech) rather than solving it. The constraint's mandate — protect speakers from harm-based suppression — sits in tension with a competing mandate — protect all groups' ability to participate in democratic and epistemic discourse. The measurement of theater_ratio (rising to 0.22 then flat) suggests institutional energy is now spent on maintaining the boundary rhetoric rather than responding to new suppression modes; this is consistent with mandate creep: the constraint persists by inertia and institutional commitment, not because the founding problem remains the primary challenge. A reclassification to piton is not warranted yet (the categorical-exclusions enforcement is still functional, not purely performative), but the mandatrophy tension is real and documented in the omegas.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    speech_as_subordination_capacity,
    'Does speech function as structural subordination of target groups, and if so, does that capacity constitute grounds for restriction compatible with absolutist protection?',
    'Empirical documentation of speech''s silencing effects on targeted groups (reduced participation, epistemic marginalization, exit from discourse) combined with normative adjudication of whether subordination-capacity overrides speaker autonomy in absolutist doctrine. Natural experiment: jurisdictions that restrict subordinating speech and measure downstream participation of targeted groups.',
    'If speech demonstrably subordinates and subordination is deemed incompatible with absolutist protection, absolutism INFLUENCES the dignity and democratic-participation readings by requiring they account for subordination-as-harm. If subordination is deemed outside absolutism''s scope (pure speaker autonomy wins), the dignity reading FORECLOSES because its core premise (speech-as-subordination is grounds for restriction) contradicts absolutism''s. The boundary between these outcomes determines whether absolutism remains holdable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speech_as_subordination_capacity, conceptual, 'Whether speech''s capacity to subordinate target groups is compatible with absolutist speaker-autonomy protection.').

omega_variable(
    government_suppression_via_harm_claims,
    'Is suppression-via-harm-claims a live threat in contemporary democracies, or has the threat shifted to speech-as-subordination and the absolutist boundary now enables rather than prevents suppression?',
    'Comparative analysis of legislative and judicial speech restrictions: are harm-based restrictions primarily used to suppress dissent/heterodoxy, or to protect targeted groups from subordinating speech? Historical trend analysis of absolutist doctrine''s evolution in response to harm-threshold challenges.',
    'If harm-based restrictions are primarily deployed against dissent, absolutism solves its founding problem. If restrictions are primarily deployed to protect targeted groups and absolutism blocks those protections, the founding problem has inverted — absolutism now enables suppression of marginalized voices'' participation-capacity. This determines whether the constraint''s mandate persists or has become obsolete (mandatrophy).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(government_suppression_via_harm_claims, empirical, 'Whether the absolutist founding problem (suppression-via-harm-claims) remains live or has been superseded by speech-as-subordination concerns.').

omega_variable(
    categorical_exclusions_stability,
    'Can the narrow categorical exclusions (true threats, imminent incitement, obscenity, child exploitation) remain stable under pressure to expand them, or does absolutism''s logic force expansion or collapse?',
    'Track legislative and judicial attempts to expand categories; analyze whether absolutist courts hold the line or yield to pressure. Examine whether the categories themselves (especially ''true threat'' and ''obscenity'') drift under social pressure or remain bounded.',
    'If categories collapse or expand substantially, absolutism fails to deliver its promised stability and may devolve into harm-threshold reasoning under pressure (effective reclassification to contested/contested-but-eroding). If categories hold, absolutism retains its boundary-integrity. The suppression_requirement trajectory depends on this — flat suppression means the boundary holds; rising suppression means it is under sustained attack.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_exclusions_stability, empirical, 'Whether the narrow categorical exclusions can remain stable or whether absolutism''s boundaries erode under pressure.').

omega_variable(
    reading_alternatives_interpretive_tradition,
    'Within the constitutional tradition, is absolutism grounded in a genuine interpretive lineage (originalism, natural law, textualism) or is it a policy choice dressed as interpretation?',
    'Genealogical analysis of absolutist doctrine: trace it to founding-era statements, textual grounding, and sustained interpretive communities. Compare against harm-threshold and dignity readings'' interpretive lineages. Assess whether absolutism has an equal claim to the tradition or is a revisionist reading.',
    'If absolutism''s interpretive grounding is weaker than competitors, its authority_grounding shifts from ''lineage'' to ''extraction'' (institutional power wielding a favored reading). If grounding is equal or stronger, absolutism retains lineage legitimacy. This determines cs_structure fields: authority_grounding, reference_frame, drift_state direction (whether the kernel is drifting away from absolutism or absolutism is failing to hold the line).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_alternatives_interpretive_tradition, conceptual, 'Whether absolutism is grounded in constitutional tradition or is an institutional choice presented as tradition.').

omega_variable(
    kernel_reading_contest_resolution_path,
    'Can absolutism coexist with dignity and harm-threshold readings within a single constitutional framework, or does one reading eventually dominate?',
    'Long-term institutional trajectory: do courts continue to recognize absolutism, or do they adopt a more complex multi-factor test that includes dignity and harm-threshold? Comparative observation across democracies: do some stabilize pluralism while others resolve toward one reading?',
    'If pluralism is sustainable, absolutism remains a live reading (coexists_with status). If one reading dominates, absolutism either forecloses others (low probability) or is foreclosed by them (higher probability, given subordination-as-harm visibility). This determines the terminal state of the kernel contest.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest_resolution_path, conceptual, 'Whether the speech-protection kernel stabilizes around multiple coexisting readings or resolves toward one dominant reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__absolutist_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__absolutist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(spee_tr_t0, observed).
narrative_ontology:measurement(spee_tr_t12, speech_protection_kernel__absolutist_reading, theater_ratio, 12, 0.11).
narrative_ontology:measurement_basis(spee_tr_t12, observed).
narrative_ontology:measurement(spee_tr_t25, speech_protection_kernel__absolutist_reading, theater_ratio, 25, 0.14).
narrative_ontology:measurement_basis(spee_tr_t25, observed).
narrative_ontology:measurement(spee_tr_t37, speech_protection_kernel__absolutist_reading, theater_ratio, 37, 0.18).
narrative_ontology:measurement_basis(spee_tr_t37, observed).
narrative_ontology:measurement(spee_tr_t50, speech_protection_kernel__absolutist_reading, theater_ratio, 50, 0.21).
narrative_ontology:measurement_basis(spee_tr_t50, observed).
narrative_ontology:measurement(spee_tr_t62, speech_protection_kernel__absolutist_reading, theater_ratio, 62, 0.22).
narrative_ontology:measurement_basis(spee_tr_t62, observed).
narrative_ontology:measurement(spee_tr_t75, speech_protection_kernel__absolutist_reading, theater_ratio, 75, 0.22).
narrative_ontology:measurement_basis(spee_tr_t75, observed).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__absolutist_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(spee_be_t0, observed).
narrative_ontology:measurement(spee_be_t12, speech_protection_kernel__absolutist_reading, base_extractiveness, 12, 0.52).
narrative_ontology:measurement_basis(spee_be_t12, observed).
narrative_ontology:measurement(spee_be_t25, speech_protection_kernel__absolutist_reading, base_extractiveness, 25, 0.58).
narrative_ontology:measurement_basis(spee_be_t25, observed).
narrative_ontology:measurement(spee_be_t37, speech_protection_kernel__absolutist_reading, base_extractiveness, 37, 0.64).
narrative_ontology:measurement_basis(spee_be_t37, observed).
narrative_ontology:measurement(spee_be_t50, speech_protection_kernel__absolutist_reading, base_extractiveness, 50, 0.66).
narrative_ontology:measurement_basis(spee_be_t50, observed).
narrative_ontology:measurement(spee_be_t62, speech_protection_kernel__absolutist_reading, base_extractiveness, 62, 0.67).
narrative_ontology:measurement_basis(spee_be_t62, observed).
narrative_ontology:measurement(spee_be_t75, speech_protection_kernel__absolutist_reading, base_extractiveness, 75, 0.68).
narrative_ontology:measurement_basis(spee_be_t75, observed).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__absolutist_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(spee_su_t0, observed).
narrative_ontology:measurement(spee_su_t12, speech_protection_kernel__absolutist_reading, suppression_requirement, 12, 0.46).
narrative_ontology:measurement_basis(spee_su_t12, observed).
narrative_ontology:measurement(spee_su_t25, speech_protection_kernel__absolutist_reading, suppression_requirement, 25, 0.5).
narrative_ontology:measurement_basis(spee_su_t25, observed).
narrative_ontology:measurement(spee_su_t37, speech_protection_kernel__absolutist_reading, suppression_requirement, 37, 0.53).
narrative_ontology:measurement_basis(spee_su_t37, observed).
narrative_ontology:measurement(spee_su_t50, speech_protection_kernel__absolutist_reading, suppression_requirement, 50, 0.55).
narrative_ontology:measurement_basis(spee_su_t50, observed).
narrative_ontology:measurement(spee_su_t62, speech_protection_kernel__absolutist_reading, suppression_requirement, 62, 0.55).
narrative_ontology:measurement_basis(spee_su_t62, observed).
narrative_ontology:measurement(spee_su_t75, speech_protection_kernel__absolutist_reading, suppression_requirement, 75, 0.55).
narrative_ontology:measurement_basis(spee_su_t75, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__absolutist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_kernel__absolutist_reading, 0.12).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__democratic_participation_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__marketplace_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested speech-protection kernel. The kernel describes a single standing commitment to protect expression from restriction; the five readings (absolutist, democratic-participation, dignity, harm-threshold, marketplace) instantiate different boundaries and justifications. Each reading is a separate constraint story with its own ε, beneficiary/victim structure, and classification. The family is linked through network.affects_constraints: absolutism INFLUENCES all four sibling readings by setting the burden-of-proof high for any restriction and by establishing speaker autonomy as the default good. Each sibling reading either COEXISTS with absolutism (held by different constitutional scholars/judges simultaneously) or INFLUENCES it (creates structural pressure to reconsider the boundary). The kernel contest is about which reading's axioms should govern the protection boundary. No single reading has foreclosed the others within contemporary constitutional law, but subordination-as-harm visibility (documented in dignity and harm-threshold readings) creates sustained pressure on absolutism's holdability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
