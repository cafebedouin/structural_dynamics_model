% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__marketplace_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
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
 *   constraint_id: speech_protection_kernel__marketplace_reading
 *   human_readable: Marketplace-of-Ideas Reading of Speech Protection
 *   domain: constitutional law/political philosophy/communication rights
 *
 * SUMMARY:
 *   In jurisdictions that instantiate the marketplace reading, speech
 *   protection is justified by its contribution to collective
 *   truth-discovery: error must be tolerated because today's error may
 *   contain tomorrow's truth, official selection among viewpoints distorts
 *   the discovery process, and the remedy for false or harmful expression is
 *   more speech rather than enforced silence. The standing arrangement this
 *   story assesses is that protection regime — courts striking content-based
 *   restrictions, legislatures stripped of remedies their constituents
 *   demand, speakers holding broad expressive license, and the targets of
 *   protected harmful speech left to self-help remedies of uneven reach. This
 *   file is ONE READING of the speech_protection_kernel; the absolutist,
 *   harm-threshold, dignity, and democratic-participation readings are
 *   separate constraints with their own files, their own beneficiary/victim
 *   structures, and their own epsilon values over the same referent. Epsilon
 *   here is authored as the marketplace reading itself assesses the standing
 *   regime: predominantly coordinative, with real, growing, unequally
 *   distributed costs that the reading acknowledges but prices as the cost of
 *   discovery.
 *
 * KEY AGENTS:
 *   - - constitutional_high_court: Agenda setter (institutional/constrained) — administers and enforces the protection doctrine
 *   - - dissenting_speakers: Primary beneficiary (moderate/constrained) — shielded minority expression
 *   - - political_out_groups: Beneficiary (organized/constrained) — protected while out of power
 *   - - mass_listening_public: Net beneficiary carrying diffuse costs (moderate/mobile)
 *   - - large_media_platforms_and_publishers: Commercial beneficiary (powerful/arbitrage)
 *   - - targeted_speech_victims: Primary target (powerless/trapped) — bears the unremedied harm of protected speech
 *   - - unamplified_respondents: Target (powerless/trapped) — the more-speech remedy presupposes reach they lack
 *   - - elected_legislative_majorities: Payer with secondary beneficiary position (powerful/constrained)
 *   - - chilled_nonparticipating_communities: Excluded voice (powerless/identity_locked) — formally protected, practically silent
 *   - - comparative_constitutional_scholars: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__marketplace_reading, 0.44).
domain_priors:suppression_score(speech_protection_kernel__marketplace_reading, 0.66).
domain_priors:theater_ratio(speech_protection_kernel__marketplace_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__marketplace_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__marketplace_reading, "Marketplace-of-Ideas Reading of Speech Protection").
narrative_ontology:topic_domain(speech_protection_kernel__marketplace_reading, "constitutional law/political philosophy/communication rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__marketplace_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__marketplace_reading, '1f57c409-f0c6-401e-8a93-af6208f39153').
narrative_ontology:cs_kernel_codification('1f57c409-f0c6-401e-8a93-af6208f39153', fixed_text).
narrative_ontology:cs_authority_grounding('1f57c409-f0c6-401e-8a93-af6208f39153', lineage).
narrative_ontology:cs_interpretation_layer_present('1f57c409-f0c6-401e-8a93-af6208f39153').
narrative_ontology:cs_reading_relation('1f57c409-f0c6-401e-8a93-af6208f39153', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f57c409-f0c6-401e-8a93-af6208f39153', speech_protection_kernel__harm_threshold_reading, influences).
narrative_ontology:cs_reading_relation('1f57c409-f0c6-401e-8a93-af6208f39153', speech_protection_kernel__dignity_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f57c409-f0c6-401e-8a93-af6208f39153', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('1f57c409-f0c6-401e-8a93-af6208f39153', foundational, error_tolerance_required_for_discovery).
narrative_ontology:cs_axiom_status(error_tolerance_required_for_discovery, holdable).
narrative_ontology:cs_axiom_grounding('1f57c409-f0c6-401e-8a93-af6208f39153', error_tolerance_required_for_discovery, empirically_contingent).
narrative_ontology:cs_axiom('1f57c409-f0c6-401e-8a93-af6208f39153', foundational, more_speech_is_the_remedy).
narrative_ontology:cs_axiom_status(more_speech_is_the_remedy, holdable).
narrative_ontology:cs_axiom_grounding('1f57c409-f0c6-401e-8a93-af6208f39153', more_speech_is_the_remedy, instrumental).
narrative_ontology:cs_axiom('1f57c409-f0c6-401e-8a93-af6208f39153', secondary, official_viewpoint_selection_presumptively_distorting).
narrative_ontology:cs_axiom_status(official_viewpoint_selection_presumptively_distorting, holdable).
narrative_ontology:cs_axiom_grounding('1f57c409-f0c6-401e-8a93-af6208f39153', official_viewpoint_selection_presumptively_distorting, empirically_contingent).
narrative_ontology:cs_reference_frame('1f57c409-f0c6-401e-8a93-af6208f39153', content_neutral_open_contestation_baseline).
narrative_ontology:cs_drift_state('1f57c409-f0c6-401e-8a93-af6208f39153', algorithmic_amplification_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1f57c409-f0c6-401e-8a93-af6208f39153', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__marketplace_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, dissenting_speakers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, political_out_groups).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, mass_listening_public).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, large_media_platforms_and_publishers).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, targeted_speech_victims).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, unamplified_respondents).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, elected_legislative_majorities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, elected_legislative_majorities).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, mass_listening_public).
narrative_ontology:constraint_vindicates(speech_protection_kernel__marketplace_reading, truth_discovery_through_open_contestation).
narrative_ontology:constraint_vindicates(speech_protection_kernel__marketplace_reading, millian_fallibilism).
narrative_ontology:constraint_vindicates(speech_protection_kernel__marketplace_reading, content_neutrality_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the protection doctrine case by case: strikes content-based restrictions, polices the carve-outs, and writes the opinions that define what counts as viewpoint discrimination. Its authority in this domain is built from the doctrine it enforces, and it absorbs the legitimacy cost each time it overturns a popular statute.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, constitutional_high_court, agenda_setter,
    institutional, generational, constrained, national).

% Hold and publish views that start as minority positions — antiwar argument, labor organizing, scientific heterodoxy, whistleblower disclosure. Judicial protection keeps their expression available while majorities are hostile, giving their claims time to be tested in public. Without that protection their publication depends on the tolerance of whoever holds office.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, dissenting_speakers, beneficiary,
    moderate, biographical, constrained, national).

% Organize and broadcast while out of power, relying on the same protection their opponents relied on when they were out. When they win office they inherit the doctrine that bound their predecessors and chafes at their own agenda; the arrangement binds whichever faction governs, so no faction can step outside it.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, political_out_groups, beneficiary,
    organized, generational, constrained, national).

% Receives the output of open contestation: exposed scandals, corrected errors, competing policy arguments. Also absorbs falsehood, manipulation, and noise, and answers bad content mainly with its own attention choices and occasional counterspeech. It cannot leave the shared information environment, only rearrange a personal media diet within it.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, mass_listening_public, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__marketplace_reading, mass_listening_public, payer).

% Operate channels whose reach and revenue depend on wide expressive latitude. Litigate to extend protection, absorb moderation-pressure costs, and monetize attention — including attention drawn by inflammatory protected content. They operate across jurisdictions and can shift investment when any single regime tightens.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, large_media_platforms_and_publishers, beneficiary,
    powerful, biographical, arbitrage, global).

% Are harassed, vilified, or defamed by expression the courts decline to restrict — group-directed hostility, coordinated pile-ons, damaging falsehoods below the legal threshold for action. The remedy they request is restriction; the doctrine forecloses it. They cannot leave the national discourse in which they are targeted, and answering back requires reach most of them lack.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, targeted_speech_victims, payer,
    powerless, biographical, trapped, national).

% Ordinary individuals caught in viral falsehoods or mass criticism. The prescribed response — answer with more speech — presumes an audience; corrections typically circulate to a fraction of the original's viewers. Their reputation absorbs the difference.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, unamplified_respondents, payer,
    powerless, immediate, trapped, national).

% Enact restrictions their constituents demand — hate-speech bans, disinformation rules, harassment statutes — and watch the content-based provisions fall to judicial review. They keep content-neutral tools but lose the specific remedies voters asked for; their own members' speech enjoys the same protection whenever they are out of power.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, elected_legislative_majorities, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__marketplace_reading, elected_legislative_majorities, beneficiary).

% Would join public argument but stay out: hostile climate, prior targeting of their group, and a reasonable expectation that speaking invites attack. Their formal right to speak is intact; their actual presence in the conversation is not. Leaving the discourse would mean abandoning the community whose standing they seek.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, chilled_nonparticipating_communities, excluded,
    powerless, generational, identity_locked, national).

% Study how jurisdictions with different speech settlements perform on accuracy, participation, and harm. Publish comparisons, testify, and advise reformers; hold no enforcement power and collect nothing from the arrangement.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__marketplace_reading, large_media_platforms_and_publishers).
narrative_ontology:fixing_cost_class(speech_protection_kernel__marketplace_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the epistemic commons problem: keeping unpopular and erroneous expression available despite transient majoritarian hostility, so that error can be detected and corrected through open contestation instead of official selection among viewpoints.
% TRANSFER_FUNCTION: Moves remedial authority from targeted listeners and legislative majorities to speakers and adjudicating courts: targets surrender the restriction remedy, legislatures surrender content-based regulatory tools, speakers gain broad expressive license, and courts gain review authority over the boundary.
% ABSENT_VOICES: Chilled nonparticipating communities are formally inside the protection but practically outside the conversation it constitutes; they would object that a right they cannot exercise is not protection. Harm-threshold and dignity advocates operate in other jurisdictions' frameworks and appear here mainly as losing litigants and comparative critics.
% DISAPPEARANCE_RATIONALE: Restriction statutes would proliferate within a single legislative session; speakers would face official judgment about viewpoint; publishers and platforms would litigate in a world with no doctrine behind them; the epistemic environment would reorganize around whatever each government tolerates, and dissenting minorities would lose the shelter that lets unpopular claims survive long enough to be tested.
% FOUNDING_PROBLEM: Early twentieth-century democracies were criminalizing dissent — sedition prosecutions, loyalty purges, suppression of antiwar and labor speech. The arrangement was built to protect unpopular political expression from majoritarian and wartime suppression so that error could be contested rather than silenced.
% FOUNDING_PROBLEM_CORROBORATION: Civil-liberties monitoring organizations and comparative constitutional scholarship corroborate that state suppression of dissent remains a recurring, real problem. Platform-governance research and testimony from targeted communities corroborate the counter-claim that the operative silencing has partly migrated to private moderation and climate effects the doctrine does not reach. Corroboration exists on both sides from sources outside the doctrine's beneficiaries; no attesting source speaks for the arrangement itself.
narrative_ontology:disappearance_verdict(speech_protection_kernel__marketplace_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__marketplace_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__marketplace_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_kernel__marketplace_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__marketplace_reading, 0.44, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness (0.44) is moderate and rising: the regime's costs — unremedied harassment, group vilification, viral defamation — scale with the reach of the speech environment, so the burden borne by targets grew as distribution moved from pamphlet to broadcast to algorithmic feed even though the doctrine's text did not change. Suppression (0.66) is high because the regime's persistence requires continuous judicial enforcement against majoritarian preference: the alternative most targets and most legislatures prefer — restriction — is actively foreclosed, case after case. Theater ratio (0.38) has grown with ceremonial defense of the marketplace and with counterspeech rituals that function as prescription rather than remedy. Accessibility collapse (0.40): alternatives remain live — carve-out regimes, other jurisdictions' settlements, rival readings of the same constitutional text — so understanding the doctrine does not close the option space. Resistance (0.58): every legislative session produces new restriction proposals and victim-advocacy coalitions press for carve-outs; the doctrine is permanently contested rather than settled. All three tracked series share one time grid (1919–2025, seven points) so temporal analysis samples every metric at every point; the suppression series traces the enforcement arc (weak before the 1930s consolidation, heavy through the Cold War suppression fights, elevated again in the disinformation-regulation era) rather than a static picture.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same structure. From the bench, the doctrine is the constitution functioning as designed — each struck statute is the system working. From a targeted-minority seat, the identical doctrine is the reason a concrete, recurring harm has no remedy; from the legislature's seat it is a standing veto over tools constituents demanded; from the dissenter's seat it is the only thing standing between their speech and criminalization. Same text, same cases, opposite lived arrangements. Coalition potential matters for the powerless seats: harassment-law coalitions are exactly the payer-side combination the structure most resists, and their repeated statutory defeats are the visible trace of that pressure.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared roles drive the derivation without overrides. Dissenting speakers, out-groups, the listening public, and platform publishers sit near the beneficiary pole — the regime subsidizes their expression; the public sits nearer symmetric because it also absorbs the misinformation externality. Targeted victims and unamplified respondents sit near the full-target pole: they bear the regime's costs with trapped exit and no compensating license they value. Legislative majorities derive high d as payers of foreclosed regulatory authority, tempered by their members' own protected speech. The court derives low d as administrator — it collects no rents, but its authority in this domain is constituted by the doctrine, so it is not a neutral bystander either. No directionality overrides are authored: role plus exit options already separate the seats, and no two agents sharing a power atom diverge enough to justify a per-atom correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — protecting unpopular political expression from state suppression — is episodically alive but no longer the whole story: a large share of contemporary silencing runs through private moderation infrastructure the doctrine does not govern, and through climate effects that never required a statute. Treating the arrangement as pure coordination would hide the unremedied-harm burden that falls on identifiable people; treating it as pure extraction would erase the epistemic shelter that dissenters and out-groups demonstrably use. The hybrid classification keeps both facts load-bearing. Mandatrophy is not resolved: the mandate is contested rather than dead, and the measurement series shows the contest moving (rising theater, rising extraction) rather than settling.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_membership,
    'This constraint is the marketplace reading of the speech_protection_kernel; how would instantiating a sibling reading (absolutist, harm_threshold, dignity, democratic_participation) change the structural facts?',
    'Author the sibling stories and compare victim sets, epsilon, and enforcement requirements across readings over the same referent.',
    'Harm-threshold and dignity readings move targeted_speech_victims from the paying side to the protected side and move the burden onto speakers; the absolutist reading removes the carve-outs and lowers enforcement demand; the democratic-participation reading narrows the protected class to politically necessary expression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_membership, conceptual, 'Committer structure: which kernel, which reading, what siblings would change.').

omega_variable(
    disagreement_location_harm_treatment,
    'The readings disagree at one specific structural element: the treatment of demonstrable harm. This reading holds harm-tolerance epistemically necessary; sibling readings make protection conditional on harm absence. Is that disagreement irreducible within one framework?',
    'Normative analysis of whether harm-conditionality can be reconciled with error-tolerance without collapsing into one of the two poles.',
    'If reconcilable, the readings are variants of one constraint; if irreducible, they are distinct constraints with disjoint victim sets and the classification of each stands alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_location_harm_treatment, conceptual, 'Where the kernel contest is located: harm treatment.').

omega_variable(
    counterspeech_remedy_reach_asymmetry,
    'Does the more-speech remedy actually reach the audiences of harmful speech for resource-poor responders, or does correction systematically fail to match falsehood distribution?',
    'Platform diffusion studies comparing correction versus falsehood reach; audit studies of response capacity across resource levels.',
    'Systematic failure for a class of targets concentrates the burden on that class and pushes per-seat classifications toward the target pole; roughly uniform success supports the coordination framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterspeech_remedy_reach_asymmetry, empirical, 'Whether the prescribed remedy functions for those without amplification.').

omega_variable(
    convergence_premise_under_amplification,
    'Does open contestation still converge on truth under algorithmic amplification, or has engagement-optimized distribution broken the discovery mechanism the doctrine presupposes?',
    'Longitudinal epistemic-quality studies; natural experiments from jurisdictional variation in restriction regimes.',
    'If broken, the coordination function decays while the burden persists — drift toward piton or snare; if intact, the rising extraction series reflects scale, not decay.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(convergence_premise_under_amplification, empirical, 'Status of the doctrine''s foundational empirical premise under modern distribution.').

omega_variable(
    epistemic_benefit_conditionality,
    'Is the truth-discovery benefit of broad protection intrinsic to any free society, or contingent on institutional conditions — functioning press, shared epistemic standards, literate publics — that may not hold?',
    'Comparative historical analysis of speech regimes and epistemic outcomes under varying institutional conditions.',
    'If contingent, the arrangement is transitional and its justification weakens as conditions erode; if intrinsic, it is a permanent load-bearing structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemic_benefit_conditionality, conceptual, 'Whether the coordination benefit is unconditional or institutionally conditional.').

omega_variable(
    formal_protection_vs_participation_equality,
    'Does formal expressive protection translate into actual participation equality, or does hostile climate exclude identifiable communities despite their formal right?',
    'Participation-gap studies by group; survey evidence on self-censorship correlated with targeted hostility.',
    'Large persistent gaps mean the excluded seat is systematic rather than incidental — the arrangement coordinates speakers while failing the silenced, deepening the hybrid reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_protection_vs_participation_equality, empirical, 'Whether formal protection reaches the chilled.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__marketplace_reading, 1919, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spk_marketplace_tr_t1919, speech_protection_kernel__marketplace_reading, theater_ratio, 1919, 0.12).
narrative_ontology:measurement(spk_marketplace_tr_t1937, speech_protection_kernel__marketplace_reading, theater_ratio, 1937, 0.15).
narrative_ontology:measurement(spk_marketplace_tr_t1954, speech_protection_kernel__marketplace_reading, theater_ratio, 1954, 0.18).
narrative_ontology:measurement(spk_marketplace_tr_t1969, speech_protection_kernel__marketplace_reading, theater_ratio, 1969, 0.16).
narrative_ontology:measurement(spk_marketplace_tr_t1990, speech_protection_kernel__marketplace_reading, theater_ratio, 1990, 0.22).
narrative_ontology:measurement(spk_marketplace_tr_t2012, speech_protection_kernel__marketplace_reading, theater_ratio, 2012, 0.31).
narrative_ontology:measurement(spk_marketplace_tr_t2025, speech_protection_kernel__marketplace_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(spk_marketplace_be_t1919, speech_protection_kernel__marketplace_reading, base_extractiveness, 1919, 0.22).
narrative_ontology:measurement(spk_marketplace_be_t1937, speech_protection_kernel__marketplace_reading, base_extractiveness, 1937, 0.24).
narrative_ontology:measurement(spk_marketplace_be_t1954, speech_protection_kernel__marketplace_reading, base_extractiveness, 1954, 0.28).
narrative_ontology:measurement(spk_marketplace_be_t1969, speech_protection_kernel__marketplace_reading, base_extractiveness, 1969, 0.32).
narrative_ontology:measurement(spk_marketplace_be_t1990, speech_protection_kernel__marketplace_reading, base_extractiveness, 1990, 0.36).
narrative_ontology:measurement(spk_marketplace_be_t2012, speech_protection_kernel__marketplace_reading, base_extractiveness, 2012, 0.41).
narrative_ontology:measurement(spk_marketplace_be_t2025, speech_protection_kernel__marketplace_reading, base_extractiveness, 2025, 0.44).

% Suppression requirement over time
narrative_ontology:measurement(spk_marketplace_su_t1919, speech_protection_kernel__marketplace_reading, suppression_requirement, 1919, 0.2).
narrative_ontology:measurement(spk_marketplace_su_t1937, speech_protection_kernel__marketplace_reading, suppression_requirement, 1937, 0.35).
narrative_ontology:measurement(spk_marketplace_su_t1954, speech_protection_kernel__marketplace_reading, suppression_requirement, 1954, 0.55).
narrative_ontology:measurement(spk_marketplace_su_t1969, speech_protection_kernel__marketplace_reading, suppression_requirement, 1969, 0.62).
narrative_ontology:measurement(spk_marketplace_su_t1990, speech_protection_kernel__marketplace_reading, suppression_requirement, 1990, 0.58).
narrative_ontology:measurement(spk_marketplace_su_t2012, speech_protection_kernel__marketplace_reading, suppression_requirement, 2012, 0.6).
narrative_ontology:measurement(spk_marketplace_su_t2025, speech_protection_kernel__marketplace_reading, suppression_requirement, 2025, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__marketplace_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'free speech' decomposes into five structurally distinct commitments sharing one constitutional referent. This story carries the marketplace reading's epsilon, victim set, and enforcement profile; the sibling files carry theirs. This reading is the entrenched baseline whose persistence raises the justification burden on harm-threshold settlements elsewhere (influences edge), while the remaining readings coexist as live positions no single framework eliminates. Cross-file comparison of victim sets is the intended consumption: harm-threshold and dignity readings move targeted_speech_victims from the paying side to the protected side and move the burden onto speakers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
