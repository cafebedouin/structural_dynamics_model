% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__marketplace_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: speech_protection_kernel__marketplace_reading
 *   human_readable: Speech Protection via Marketplace Truth-Discovery (Marketplace Reading)
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   The marketplace reading of speech protection justifies broad speech
 *   protection as a collective epistemic benefit: truth emerges through
 *   competitive discourse, and content-based restrictions distort that
 *   process. This reading is one of five coherent framings of the
 *   speech-protection kernel. It privileges the mechanism of truth-discovery
 *   over the protection of individual autonomy (absolutist reading),
 *   democratic participation requirements (democratic_participation reading),
 *   dignity of targeted groups (dignity reading), or empirical harm
 *   thresholds (harm_threshold reading). The marketplace reading operates as
 *   a tangled rope: it coordinates genuine epistemic function (open discourse
 *   enables distributed fact-checking and correction) while extracting from
 *   those who lack resources to participate in counterspeech production. It
 *   requires active enforcement to exclude rivals (content-restriction
 *   advocates, harm-reduction frameworks) and suppress the alternative
 *   remedies they propose.
 *
 * KEY AGENTS:
 *   - institutional_speakers: Hold enormous speech reach and resources for rebuttal; benefit from protection of their false/contested claims under the assumption counterspeech can answer them
 *   - targets_of_false_speech: Bear the cost of the remedy (must produce counterspeech); have constrained resources and exit options
 *   - marginalized_voices: Structurally excluded from the decision frame; subject to coordinated false speech campaigns; the remedy (produce more speech) assumes resources they lack
 *   - platform_operators: Set and enforce the rule; benefit from cost-externalization to counterspeech producers
 *   - fact_checking_institutions: Implement the constraint operationally; dependent on the constraint's logic for their institutional role
 *   - content_restriction_advocates: Excluded parties who argue the marketplace assumption fails empirically
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__marketplace_reading, 0.68).
domain_priors:suppression_score(speech_protection_kernel__marketplace_reading, 0.71).
domain_priors:theater_ratio(speech_protection_kernel__marketplace_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__marketplace_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__marketplace_reading, "Speech Protection via Marketplace Truth-Discovery (Marketplace Reading)").
narrative_ontology:topic_domain(speech_protection_kernel__marketplace_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__marketplace_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__marketplace_reading, '5fb689fa-b3e3-455e-9b46-30603530bf7b').
narrative_ontology:cs_kernel_codification('5fb689fa-b3e3-455e-9b46-30603530bf7b', fixed_text).
narrative_ontology:cs_authority_grounding('5fb689fa-b3e3-455e-9b46-30603530bf7b', lineage).
narrative_ontology:cs_interpretation_layer_present('5fb689fa-b3e3-455e-9b46-30603530bf7b').
narrative_ontology:cs_reading_relation('5fb689fa-b3e3-455e-9b46-30603530bf7b', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('5fb689fa-b3e3-455e-9b46-30603530bf7b', speech_protection_kernel__harm_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('5fb689fa-b3e3-455e-9b46-30603530bf7b', speech_protection_kernel__dignity_reading, coexists_with).
narrative_ontology:cs_reading_relation('5fb689fa-b3e3-455e-9b46-30603530bf7b', speech_protection_kernel__democratic_participation_reading, influences).
narrative_ontology:cs_axiom('5fb689fa-b3e3-455e-9b46-30603530bf7b', foundational, truth_emerges_through_speech_competition).
narrative_ontology:cs_axiom_status(truth_emerges_through_speech_competition, holdable).
narrative_ontology:cs_axiom_grounding('5fb689fa-b3e3-455e-9b46-30603530bf7b', truth_emerges_through_speech_competition, empirically_contingent).
narrative_ontology:cs_axiom('5fb689fa-b3e3-455e-9b46-30603530bf7b', foundational, content_restriction_distorts_epistemic_process).
narrative_ontology:cs_axiom_status(content_restriction_distorts_epistemic_process, holdable).
narrative_ontology:cs_axiom_grounding('5fb689fa-b3e3-455e-9b46-30603530bf7b', content_restriction_distorts_epistemic_process, instrumental).
narrative_ontology:cs_reference_frame('5fb689fa-b3e3-455e-9b46-30603530bf7b', open_discourse_epistemic_sufficiency).
narrative_ontology:cs_drift_state('5fb689fa-b3e3-455e-9b46-30603530bf7b', networked_communication_scale, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5fb689fa-b3e3-455e-9b46-30603530bf7b', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(speech_protection_kernel__marketplace_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, institutional_speakers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, resource_advantaged_speakers).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, targets_of_false_speech).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, marginalized_voices).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, fact_checking_and_counterspeech_institutions).
narrative_ontology:constraint_vindicates(speech_protection_kernel__marketplace_reading, epistemic_benefit_from_open_discourse).
narrative_ontology:constraint_vindicates(speech_protection_kernel__marketplace_reading, truth_converges_through_market_competition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Media organizations, universities, corporations, government bodies. Operate under the reading's protection: their speech (even false speech) is protected by the principle that counterspeech, not suppression, is the remedy. They have resources to fund rapid rebuttal and alternative narratives. They benefit from the asymmetry: institutional speech reaches wide audiences; counterspeech directed at them exists but fragments across smaller platforms and less-trusted sources.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, institutional_speakers, beneficiary,
    institutional, generational, arbitrage, national).

% Well-funded individuals, corporations, political organizations with communication budgets. Can deploy counterspeech themselves or hire others to do so. Can sustain a narrative over time through repeated messaging. The marketplace logic assumes they can afford to compete in speech volume and reach; the constraint protects their false or contested claims under the rationale that rebuttal is possible.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, resource_advantaged_speakers, beneficiary,
    powerful, biographical, mobile, national).

% Individuals falsely accused, defamed, or targeted by circulating falsehoods (medical misinformation applied to them, conspiracy claims about them, false criminal allegations). Under the marketplace reading, their remedy is counterspeech: they must author their own rebuttal, fund its distribution, build credibility to compete with the false narrative. They cannot petition the platform or state to suppress the false speech; they must outcompete it. This is costly and uncertain, especially against well-funded or coordinated speech.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, targets_of_false_speech, payer,
    moderate, biographical, constrained, national).

% Communities subject to coordinated false speech and hate speech campaigns (e.g., harassment targeting ethnic groups, immigration conspiracy theories, slur-based attack campaigns). The marketplace reading's logic treats their injury as a speech problem solvable by more speech; in practice, counterspeech from within marginalized groups is resource-constrained and often amplifies the attack signal (engagement). Their exclusion is not formal but structural: the constraint was authored without their input, and the remedy (produce more speech louder) assumes resources and reach they do not have.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, marginalized_voices, payer,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__marketplace_reading, marginalized_voices, excluded).

% Social media, publishing, and communication platforms. Under the marketplace reading, they are required to host false and harmful speech and leave the remedy to counterspeech. This removes pressure to invest in content moderation (costly, legally risky) and makes them neutral hosts rather than editors. They are freed from liability for user speech in the relevant jurisdictions and protected from demands to suppress specific content. They benefit from the constraint because it locks in a low-cost, legally defensible position.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, platform_operators, agenda_setter,
    institutional, generational, mobile, global).

% Fact-checkers, journalists, watchdog groups, academic institutions tasked with rebuttal and truth-establishment. Under the marketplace reading, they become the operational implementation: their speech is the remedy. They benefit from the constraint because it creates institutional demand for their work and legally protects the false speech they must then counter. They are dependent on the constraint to justify their role.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, fact_checking_and_counterspeech_institutions, beneficiary,
    organized, generational, constrained, national).

% Harm-reduction advocates, harm-threshold proponents, dignity-based restriction advocates. They argue that some speech (coordinated false health claims, incitement, targeted harassment) causes injury that counterspeech cannot undo. The marketplace reading forecloses their preferred remedy: content restriction. They are structurally excluded from the decision frame because the core axiom (truth emerges through market competition) is incompatible with their causal claim (some speech does direct harm that speech cannot repair).
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, content_restriction_advocates, excluded,
    organized, biographical, trapped, national).

% Courts, legislatures, executive regulators interpreting and enforcing speech protection law. They adjudicate whether the marketplace logic holds in specific cases and whether empirical reality matches the epistemic assumption (does truth actually converge). Their role is to validate or revise the constraint based on evidence of whether open discourse produces collective epistemic benefit or produces persistent false belief and coordinated harm.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, political_authority_institutions, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__marketplace_reading, platform_operators).
narrative_ontology:fixing_cost_class(speech_protection_kernel__marketplace_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for collective truth-discovery: by protecting false and contested speech from suppression, the reading assumes all parties can engage in rebuttal, correction, and evidence-presentation simultaneously, allowing truth to emerge through competitive discourse rather than through editorial or state gatekeeping.
% TRANSFER_FUNCTION: Transfers the burden of proof and correction from platforms/state (who would suppress false speech) to potential targets and fact-checking institutions (who must now produce counterspeech). Moves communication authority from centralized suppressors to distributed rebutters. Moves resource costs to whoever can afford sustained counterspeech campaigns.
% ABSENT_VOICES: Targets of false speech campaigns, marginalized communities subject to coordinated hate speech, and harm-reduction advocates who would restrict certain speech. These parties argue that the marketplace assumption (that truth converges through speech competition) fails empirically and that some speech causes injury counterspeech cannot repair. They are structurally absent from the marketplace reading's founding conversation and are not consulted on whether the remedy (more speech) is adequate for the harm they experience.
% DISAPPEARANCE_RATIONALE: If the marketplace reading's protection vanished and content-based restrictions became permissible, platforms and states would invest in suppression infrastructure, the cost of speech would shift from rebuttal-production to prior-approval-navigation, and institutional gatekeepers would control what narrative competes. Discourse would reorganize around centralized editorial authority rather than distributed counterspeech. The institutional beneficiaries (platforms, institutional speakers with resources) would lose the cost-free protection.
% FOUNDING_PROBLEM: Mid-20th-century problem: government censorship and monopoly control over communication channels allowed false state narratives to persist unopposed; the epistemic solution was to protect speech generally and allow competing claims to test each other in open debate rather than submitting truth to state license.
% FOUNDING_PROBLEM_CORROBORATION: The marketplace reading's institutional beneficiaries (platforms, publishers, speakers) attest the founding problem is still live: state censorship and monopolistic control of speech channels remain threats. Content-restriction advocates and targets of false speech campaigns attest the founding problem is substantially solved (democratic states do not command truth ex cathedra anymore) and the constraint now persists as protection for false speech production by resource-advantaged parties. Legal scholars outside the beneficiary set document the empirical divergence: speech competition in networked environments does not converge to truth; instead, it produces algorithmic amplification of polarizing falsehoods, identity-protective cognition, and sustained false belief among groups that encounter only consonant speech.
narrative_ontology:disappearance_verdict(speech_protection_kernel__marketplace_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__marketplace_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__marketplace_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_kernel__marketplace_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__marketplace_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__marketplace_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_kernel__marketplace_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_kernel__marketplace_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.48 to 0.68 over the interval because the empirical landscape shifts: as networked communication matures, the gap between the constraint's causal assumption (truth converges through speech competition) and observable outcomes (algorithmic amplification of polarizing falsehoods, identity-protective cognition, persistent false belief in homogeneous networks) widens. The constraint persists anyway, extracting from targets who cannot outcompete false speech and from marginalized groups lacking resources. Theater rises from 0.22 to 0.42 because an increasing share of 'counterspeech' activity becomes performative: fact-checkers and journalists produce corrections that rarely reach the audiences that encountered the false claim, identity-protective cognition prevents correction from updating belief, and the machinery of counterspeech becomes a rhetorical gesture toward epistemic benefit rather than a mechanism producing it. Suppression requirement stays high (0.71) because the constraint's persistence depends on actively excluding content-restriction frameworks and harm-reduction proposals. Without that suppressive force, the competing axioms (harm is a legitimate grounds for restriction, dignity should constrain speech, empirical harm matters) would destabilize the marketplace logic.
 *
 * PERSPECTIVAL GAP:
 *   The perspective from institutional speakers and platform operators sees a genuine coordination mechanism: open discourse enables distributed truth-seeking and protects them from arbitrary suppression. The perspective from targets of false speech campaigns sees an enforced asymmetry: their burden to refute is unpaid work; resource-advantaged speakers can fund false narratives; the remedy is not available to them in practice. The perspective from marginalized communities sees structural exclusion: the constraint was authored without their input and the remedy assumes resources (amplification, reach, sustained funding) they do not possess. The engine computes these divergences from the structural data: institutional agents with arbitrage-grade exit and abundant resources sit near the beneficiary end (low d); targets with constrained exit and moderate power sit near the target end (high d); marginalized voices with trapped exit and powerless position sit at the full-target extreme (d near 1.0). The marketplace reading's universalizing language ('truth emerges through speech') masks these asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional speakers and platform operators declare themselves beneficiaries because the constraint protects their speech from suppression (structural benefit: low d, near 0.1–0.2). Targets of false speech are victims: they cannot suppress the false narrative and must produce counterspeech at their own cost (structural harm: high d, near 0.7–0.8). Marginalized voices are both victims (subject to coordinated campaigns) and excluded (not consulted on the remedy frame): their d sits at the extreme (near 0.9) because the constraint extracts from them through identity-lock (the false narratives target their group identity; exit means silencing their group voice) and through the trap of constrained communication access. Fact-checking institutions appear as beneficiaries but are actually co-dependent payers: they appear to collect institutional authority and resources from their counterspeech role, but they are trapped in the logic (if the marketplace assumption breaks, their role becomes redundant) and their resources are insufficient to the task (they cannot outcompete well-funded false speech). Their secondary role is payer.
 *
 * MANDATROPHY ANALYSIS:
 *   The marketplace reading's founding problem (government censorship and monopolistic media gatekeeping) was a live, salient problem in mid-20th-century context. As media infrastructure decentralized and network platforms multiplied, the problem changed: instead of monopolistic suppression, the issue became algorithmic amplification of engaging (often false) content, identity-protective cognition (people resist belief-update even when correction is available), and coordinated disinformation campaigns by resource-advantaged actors. The remedy—counterspeech—remains sound as a principle but fails in operation: corrections reach smaller audiences than the false claims; audience selection effects mean people encounter consonant speech; and well-funded false narratives outpace fact-checking. The constraint persists (manifested in section 230 immunity, judicial speech-protection doctrine, platform design choices) not because the founding problem is still live but because the institutional beneficiaries (platforms, institutional speakers) have built their business models and legal strategies around the marketplace logic. The theater ratio rising from 0.22 to 0.42 indicates that enforcement increasingly consists of rhetorical gestures (producing fact-checks, issuing transparency reports, undertaking counterspeech) rather than mechanisms that produce the declared epistemic benefit. The constraint has developed a mandatrophic character: it is maintained through institutional inertia and the sunk costs of beneficiaries, not because it solves the problem it was built for.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_convergence_assumption,
    'Does truth actually converge through competitive discourse, or do networked communication systems amplify polarizing falsehoods and entrench false belief?',
    'Longitudinal empirical studies of belief-updating in response to correction and counterspeech, network analysis of information diffusion patterns, and measurement of whether audiences encounter or avoid fact-checks. Cross-jurisdictional comparison with systems using content restriction: do they produce better epistemic outcomes or trade off other values?',
    'If truth reliably converges through speech competition, the marketplace reading''s core axiom holds and the constraint functions as genuine coordination. If truth does not converge (persistent false belief, algorithmic amplification of polarizing claims), the constraint extracts from targets without delivering the epistemic benefit that justifies extraction. Reclassification would shift toward snare (pure extraction) unless alternative justifications for the protection (individual autonomy, democratic participation) are adopted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_convergence_assumption, empirical, 'Whether the foundational causal claim of the marketplace reading holds empirically.').

omega_variable(
    remedy_adequacy_for_networked_scale,
    'Is counterspeech an adequate remedy for false speech at the scale and speed of networked communication, or does the constraint''s operational logic break down when false narratives are coordinated and well-funded?',
    'Audit the reach and timing of fact-checks relative to initial false claims; measure audience overlap (do the people who encounter the false claim encounter the correction?); test whether identity-protective cognition prevents correction from updating belief even when encountered. Natural experiments: jurisdictions that adopt restrictions on certain false speech (health misinformation, election falsehood) and observe whether epistemic harms decrease or if suppression creates other problems.',
    'If counterspeech fails operationally at scale, the constraint extracts from targets without delivering the remedy it promises. Alternative remedies (platform amplification curation, fact-check prioritization, educational intervention) would become necessary, and the constraint would be forced to coexist with remedies the marketplace axiom rejects. This would move the constraint from tangled_rope (mixed coordination and extraction) toward snare (extraction without adequate remedy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedy_adequacy_for_networked_scale, empirical, 'Whether the remedy mechanism (counterspeech) functions as the constraint assumes.').

omega_variable(
    asymmetry_in_counterspeech_capacity,
    'Do resource-advantaged speakers systematically outcompete less-resourced speakers in the marketplace, creating persistent asymmetry that undermines the equal-voice assumption?',
    'Measure the relative reach, amplification, and staying power of false claims by source (institutional vs. individual, well-funded vs. grassroots, coordinated vs. distributed). Track whether targets of false speech successfully execute counterspeech campaigns or whether their rebuttal is overwhelmed. Study whether the constraint''s protection disproportionately benefits institutional speakers.',
    'If systematic asymmetry exists, the constraint functions as rent extraction by resource-advantaged speakers rather than as collective truth-seeking. The beneficiary and victim structures would be validated (institutional speakers benefit, targets and marginalized voices lose), and the classification would move toward snare (asymmetric extraction) unless the constraint''s justification is reframed to resource-neutral terms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(asymmetry_in_counterspeech_capacity, empirical, 'Whether the marketplace mechanism distributes benefits and burdens symmetrically.').

omega_variable(
    kernel_reading_foreclosure_relation,
    'Does the marketplace reading''s core axiom (truth emerges through competitive speech) logically foreclose the harm_threshold reading''s core axiom (some speech causes harm that speech cannot repair)?',
    'Logical analysis: can a single interpretive framework hold both axioms without contradiction? If harm is demonstrable and speech cannot undo it (empirical claim), does the marketplace reading''s logic permit harm-based restriction, or does it deny the harm-undoable claim a priori?',
    'If the axioms logically contradict, the readings foreclose each other and cannot coexist in a single framework (only different parties hold different readings). If they are compatible (marketplace logic can absorb a harm exception), they coexist rather than foreclose. This affects how the kernel is theorized: as a single site with incompatible readings (zero-sum contest), or as a space where different principles apply at different scales (marketplace at large scale, harm-restriction at localized scale).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_relation, conceptual, 'Structural relationship between marketplace and harm-based restrictions in the kernel reading family.').

omega_variable(
    excluded_voices_representation,
    'Would including voices of targets of false speech campaigns and marginalized communities in the decision frame change the reading''s axioms or operational logic?',
    'Structured engagement with excluded parties (harm-reduction advocates, targets of disinformation, marginalized communities) on whether counterspeech remedies their situation. Hypothetical: if the marketplace reading required demonstrated adequacy of counterspeech for all affected parties before protection was granted, would the constraint''s justification hold?',
    'If included voices demand alternative remedies (restriction, amplification curation, platform intervention), the constraint would be forced to reconcile the marketplace logic with harm-reduction logic. Either the constraint would shift type (adding enforcement of harm-reduction alongside speech-protection) or the reading would fracture into multiple seats with incompatible positions. The analysis of whether the constraint is tangled_rope (genuine coordination + extraction) or snare (pure extraction with false coordination cover) depends on whether the excluded voices'' concerns can be accommodated within the reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_voices_representation, preference, 'Whether the reading''s axioms are robust to input from currently excluded stakeholders.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__marketplace_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__marketplace_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(spee_tr_t0, observed).
narrative_ontology:measurement(spee_tr_t5, speech_protection_kernel__marketplace_reading, theater_ratio, 5, 0.27).
narrative_ontology:measurement_basis(spee_tr_t5, observed).
narrative_ontology:measurement(spee_tr_t10, speech_protection_kernel__marketplace_reading, theater_ratio, 10, 0.33).
narrative_ontology:measurement_basis(spee_tr_t10, observed).
narrative_ontology:measurement(spee_tr_t15, speech_protection_kernel__marketplace_reading, theater_ratio, 15, 0.38).
narrative_ontology:measurement_basis(spee_tr_t15, observed).
narrative_ontology:measurement(spee_tr_t20, speech_protection_kernel__marketplace_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(spee_tr_t20, observed).
narrative_ontology:measurement(spee_tr_t25, speech_protection_kernel__marketplace_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement_basis(spee_tr_t25, observed).
narrative_ontology:measurement(spee_tr_t30, speech_protection_kernel__marketplace_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(spee_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__marketplace_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(spee_be_t0, observed).
narrative_ontology:measurement(spee_be_t5, speech_protection_kernel__marketplace_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement_basis(spee_be_t5, observed).
narrative_ontology:measurement(spee_be_t10, speech_protection_kernel__marketplace_reading, base_extractiveness, 10, 0.61).
narrative_ontology:measurement_basis(spee_be_t10, observed).
narrative_ontology:measurement(spee_be_t15, speech_protection_kernel__marketplace_reading, base_extractiveness, 15, 0.65).
narrative_ontology:measurement_basis(spee_be_t15, observed).
narrative_ontology:measurement(spee_be_t20, speech_protection_kernel__marketplace_reading, base_extractiveness, 20, 0.67).
narrative_ontology:measurement_basis(spee_be_t20, observed).
narrative_ontology:measurement(spee_be_t25, speech_protection_kernel__marketplace_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(spee_be_t25, observed).
narrative_ontology:measurement(spee_be_t30, speech_protection_kernel__marketplace_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(spee_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__marketplace_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(spee_su_t0, observed).
narrative_ontology:measurement(spee_su_t5, speech_protection_kernel__marketplace_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement_basis(spee_su_t5, observed).
narrative_ontology:measurement(spee_su_t10, speech_protection_kernel__marketplace_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(spee_su_t10, observed).
narrative_ontology:measurement(spee_su_t15, speech_protection_kernel__marketplace_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(spee_su_t15, observed).
narrative_ontology:measurement(spee_su_t20, speech_protection_kernel__marketplace_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(spee_su_t20, observed).
narrative_ontology:measurement(spee_su_t25, speech_protection_kernel__marketplace_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(spee_su_t25, observed).
narrative_ontology:measurement(spee_su_t30, speech_protection_kernel__marketplace_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(spee_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__marketplace_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_kernel__marketplace_reading, 0.12).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__democratic_participation_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__dignity_reading).

% DUAL FORMULATION NOTE:
% The speech_protection_kernel family decomposes into five structurally distinct constraint stories, one per coherent reading. The kernel is the contested commitment to speech protection itself; each reading specifies a different justification (epistemic benefit, individual autonomy, democratic function, harm thresholds, group dignity) and different remedial logic. The marketplace_reading story models the empirically dominant constraint in contemporary US and platform law: protection justified by truth-discovery and remedied by counterspeech. Sibling readings have different ε values, beneficiary/victim structures, and computational types. The marketplace reading influences all siblings by establishing the legal and normative baseline against which alternatives are positioned; no sibling reading fully forecloses the marketplace reading, but each proposes structural alternatives to remedy its failures. The five stories are linked by affects_constraints edges forming a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_kernel__marketplace_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
