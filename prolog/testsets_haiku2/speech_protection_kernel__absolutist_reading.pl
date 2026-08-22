% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: speech_protection_kernel__absolutist_reading
 *   human_readable: Absolutist Speech Protection: Near-Categorical Speaker Autonomy
 *   domain: constitutional/political_philosophy
 *
 * SUMMARY:
 *   The absolutist reading of speech protection holds that listener
 *   harm—psychological, reputational, emotional, or dignitary—is
 *   categorically insufficient grounds for restricting speech. Protection
 *   operates near-completely: the only legitimate categories of exclusion are
 *   narrow, historically-settled ones (incitement to imminent lawless action,
 *   true threats, defamation with narrow definitions, child sexual abuse
 *   material). The reading prioritizes speaker autonomy and presumes that
 *   censorship regimes have historically done more harm than unrestricted
 *   speech. It therefore rejects harm-based objections as a category. This
 *   generates a structural asymmetry: speakers with unpopular views benefit
 *   greatly (near-categorical protection), while targets of hateful,
 *   harassing, or dehumanizing speech bear concentrated costs (their harm is
 *   not actionable). The claim is that this is the correct constitutional
 *   balance; the metrics describe the actual extraction burden this reading
 *   places on speech targets.
 *
 * KEY AGENTS:
 *   - absolutist_speakers: Protected beneficiaries whose speech is shielded from harm-based restriction
 *   - speech_targets_and_harmed_listeners: Identity-locked victims bearing the costs of harmful speech without legal recourse
 *   - constitutional_interpreters: Institutional agenda-setters enforcing the absolutist boundary
 *   - harm_threshold_advocates: Excluded parties who would restructure the constraint
 *   - platforms_and_publishers: Dual-positioned agents enforcing the reading while facing pressure to exceed it
 *   - democracy_advocates: Observers analyzing whether absolutism serves democratic self-governance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__absolutist_reading, 0.71).
domain_priors:suppression_score(speech_protection_kernel__absolutist_reading, 0.62).
domain_priors:theater_ratio(speech_protection_kernel__absolutist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__absolutist_reading, rope).
narrative_ontology:human_readable(speech_protection_kernel__absolutist_reading, "Absolutist Speech Protection: Near-Categorical Speaker Autonomy").
narrative_ontology:topic_domain(speech_protection_kernel__absolutist_reading, "constitutional/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_kernel__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__absolutist_reading, 'da9f8658-415b-478c-88f8-c4b3163bf627').
narrative_ontology:cs_kernel_codification('da9f8658-415b-478c-88f8-c4b3163bf627', fixed_text).
narrative_ontology:cs_authority_grounding('da9f8658-415b-478c-88f8-c4b3163bf627', lineage).
narrative_ontology:cs_interpretation_layer_present('da9f8658-415b-478c-88f8-c4b3163bf627').
narrative_ontology:cs_reading_relation('da9f8658-415b-478c-88f8-c4b3163bf627', speech_protection_kernel__harm_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('da9f8658-415b-478c-88f8-c4b3163bf627', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('da9f8658-415b-478c-88f8-c4b3163bf627', speech_protection_kernel__dignity_reading, forecloses).
narrative_ontology:cs_reading_relation('da9f8658-415b-478c-88f8-c4b3163bf627', speech_protection_kernel__democratic_participation_reading, influences).
narrative_ontology:cs_axiom('da9f8658-415b-478c-88f8-c4b3163bf627', foundational, harm_irrelevance_to_protection).
narrative_ontology:cs_axiom_status(harm_irrelevance_to_protection, holdable).
narrative_ontology:cs_axiom_grounding('da9f8658-415b-478c-88f8-c4b3163bf627', harm_irrelevance_to_protection, deontological).
narrative_ontology:cs_axiom('da9f8658-415b-478c-88f8-c4b3163bf627', foundational, speaker_autonomy_intrinsic_good).
narrative_ontology:cs_axiom_status(speaker_autonomy_intrinsic_good, holdable).
narrative_ontology:cs_axiom_grounding('da9f8658-415b-478c-88f8-c4b3163bf627', speaker_autonomy_intrinsic_good, deontological).
narrative_ontology:cs_reference_frame('da9f8658-415b-478c-88f8-c4b3163bf627', speaker_autonomy_primacy).
narrative_ontology:cs_drift_state('da9f8658-415b-478c-88f8-c4b3163bf627', digital_scale_contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('da9f8658-415b-478c-88f8-c4b3163bf627', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__absolutist_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, speakers_with_unpopular_views).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, dissidents_and_marginalized_voices).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, targets_of_hateful_speech).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, subordinated_groups).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, harassment_victims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, absolutist_speakers).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, speech_targets_and_harmed_listeners).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, platformsandpublishers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Speakers with unpopular, offensive, or minority views who benefit from near-categorical protection of their right to speak without restriction based on listener harm. They include dissidents, marginalized voices, and those whose speech would be suppressed under harm-based regimes. Their primary exit is self-censorship or relocation to less restrictive jurisdictions; their primary benefit is the right to speak without state or institutional interference.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, absolutist_speakers, beneficiary,
    moderate, biographical, mobile, national).

% Individuals and groups who are targets of hateful, harassing, dehumanizing, or subordinating speech. Under the absolutist reading, their harm—psychological, reputational, or dignitary—is not grounds for restricting the speech. Their exit options are severely constrained: they cannot leave their target identity (race, gender, religion, sexual orientation), cannot prevent encountering the speech in public discourse, and cannot access legal recourse through speech restrictions. They carry the costs of the constraint continuously.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, speech_targets_and_harmed_listeners, payer,
    powerless, biographical, identity_locked, national).

% Courts, legislatures, and constitutional scholars who enforce and interpret the absolutist reading. They set the doctrinal boundaries: what counts as categorical exclusion (incitement, true threats, defamation with narrow definitions), what does not (offensive speech, emotional distress, group harm). They face pressure from both speaker-protection advocates and harm-reduction constituencies. Their power is institutional; their time horizon is generational because constitutional doctrine accumulates slowly.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, constitutional_interpreters, agenda_setter,
    institutional, generational, constrained, national).

% Legislators, civil-rights advocates, and some scholars who argue for harm-based speech restrictions. They would restructure the constraint to permit regulation of demonstrably harmful speech. They are excluded from the absolutist reading's framework—their objections are to the foundational premise (that speaker autonomy overrides victim harm), not accommodated within it. Their vehicle for pressure is legislative change, litigation, or constitutional amendment.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, harm_threshold_advocates, excluded,
    powerful, biographical, constrained, national).

% Social media platforms and publishers operate under the absolutist reading when it is the governing legal regime, but also face pressure from users who experience harm. They enforce the reading by resisting content removal on harm grounds, but they also face market pressure and reputational cost from hosting speech that harms their users. They occupy a dual position: agents of the constraint's enforcement and targets of pressure to exceed it.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, platformsandpublishers, agenda_setter,
    institutional, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__absolutist_reading, platformsandpublishers, payer).

% Civil society organizations and theorists who frame speech protection as serving democratic self-governance. They observe the absolutist reading but may or may not endorse it; their primary concern is preserving the institutions of democratic participation, not maximizing speaker autonomy as an intrinsic good. They take testimony from other seats but operate at the level of legitimacy analysis.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, democracy_advocates, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__absolutist_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_protection_kernel__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a bright-line rule that protects speakers from state and institutional interference based on listener harm, removing the burden of case-by-case harm justification and preserving a domain of utterance immune to suppression. The coordination problem it solves is: 'How do we prevent those in power from censoring dissent by claiming harm?' The absolutist answer is: remove harm from the legitimate grounds of restriction entirely.
% TRANSFER_FUNCTION: Moves the burden of harm—psychological, reputational, dignitary—from the speaker (who faces no legal consequences) to the target and the listening public (who bear the costs of exposure to harmful speech without recourse through law). Redistributes power from those who define harm to those who claim speaker autonomy.
% ABSENT_VOICES: Targets of speech harm who lack institutional platforms or legal standing to object within the framework. Those harmed by coordinated dehumanizing campaigns, subordinated groups whose harm is collective rather than individual, and those whose exit from the harm is impossible (identity-locked targets). They would argue for restructuring the boundary between protection and harm if present, but the absolutist reading structurally excludes harm as a legitimate input.
% DISAPPEARANCE_RATIONALE: If the absolutist reading disappeared—i.e., if listener harm became a legitimate ground for speech restriction—the entire regime would reorganize: dissidents and marginalized speakers would face suppression by those who claimed harm from their words; the scope of protected speech would narrow substantially; institutional and state power would shift toward regulation rather than speaker autonomy. The constraint's disappearance would be the most significant constitutional rearrangement.
% FOUNDING_PROBLEM: The founding problem is: censorious governments and majorities have used 'harm' and 'offense' as pretexts for silencing dissent, marginalized voices, and religious or political minorities. Leaving harm as a legitimate restriction ground enables tyranny of the majority.
% FOUNDING_PROBLEM_CORROBORATION: The absolutist reading is attested by free-speech scholars (Mill, Brandenburg, modern First Amendment maximalists) and draws empirical support from historical examples of censorship (religious persecution, colonial suppression, authoritarian regimes). Harm-threshold advocates contest both the historical reading (arguing that narrow, well-defined harm restrictions do not inevitably lead to tyranny) and the empirical claim (arguing that unrestricted harmful speech can itself function as a form of tyranny). Democratic theorists outside the benefiting parties note that speech protection serves democracy but debate whether absolutism is necessary for that function.
narrative_ontology:disappearance_verdict(speech_protection_kernel__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_kernel__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__absolutist_reading, 0.71, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is measured at 0.71 because the constraint concentrates a substantial asymmetry: speakers gain near-complete protection; targets bear costs they cannot legally challenge. The measurement series shows gradual increase from 0.58 to 0.71 over the interval, reflecting accumulation of case law, platform scale, and digital speech harm as the interval progresses. Suppression (0.62) is moderate-high because the constraint requires active doctrinal and institutional suppression of harm-based objections: courts must repeatedly reject harm claims, platforms must resist pressure to moderate, legislatures face pressure to override the reading. Theater ratio rises from 0.12 to 0.28, indicating growing performative defense of the boundary as resistance from harm-threshold advocates intensifies. The constraint requires active maintenance because the excluded parties (harm advocates) continuously challenge its foundation. Accessibility_collapse (0.48) is moderate because alternatives to the absolutist reading remain theoretically available and politically live: they have institutional advocates, scholarly support, and democratic legitimacy in other jurisdictions. Resistance (0.72) is high because harm-threshold advocates mount sustained institutional pressure through legislation, litigation, and social movements. This is NOT a natural law or passive coordination; it is an enforced doctrine resisting active challenge.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (constitutional interpreters) and the beneficiary seat (absolutist speakers) compute the constraint as valuable coordination—a bright-line rule that protects dissent and prevents tyranny. The victim seat (speech targets) computes it as asymmetric extraction: their harm is systematically excluded from consideration, and their identity-locked position means they cannot exit. From the beneficiary seat, extractiveness looks like protection; from the victim seat, it looks like subordination. The engine computes this divergence from the structural data: beneficiaries get low d (near-beneficiary end of directionality spectrum), victims get high d (near-target end), and the identity_locked exit on the victim side amplifies their directionality toward full extraction. The constraint exhibits seat divergence because the same rule serves fundamentally different structural functions depending on whether one's speech is protected or one is targeted by others' speech.
 *
 * DIRECTIONALITY LOGIC:
 *   Absolutist speakers sit at or near d=0.0 (full beneficiary): they benefit from near-categorical protection and face minimal suppression. Their exit options are mobile (they can relocate to equally protective jurisdictions or adjust their speech), and their power varies (from powerless dissidents to moderate or powerful figures). Targets of harmful speech sit near d=1.0 (full target): they bear the burden of unrestricted speech, cannot exit their target identity, and have no legal recourse within the absolutist framework. Their power is powerless, their time horizon is biographical (they live with the ongoing harm), and their exit is identity_locked (they cannot stop being the target of others' speech). Constitutional interpreters sit near d=0.5 (symmetric): they benefit from the institutional prestige of enforcing constitutional doctrine, but they also bear the burden of resisting pressure from harm advocates and managing the institutional costs of a contested boundary. Harm-threshold advocates are excluded, not coordinated; their structural position is opposition to the reading itself, not participation within it.
 *
 * MANDATROPHY ANALYSIS:
 *   The absolutist reading faces a mandatrophy test: was it built to solve a live coordination problem (protecting dissent against censorious majorities), and does that problem persist? The founding problem—governments using harm as a pretext for silencing dissent—remains live in many jurisdictions and historically recurring. However, there is a secondary mandatrophy question: has the constraint acquired a secondary extractive function that persists independently of the founding problem? Digital-scale speech and coordinated harassment campaigns create harm at scale that Mill's 19th-century marketplace could not have contemplated. Some advocates argue that the constraint now operates partly as a cover story for institutional actors (platforms) who benefit from speech they are not required to moderate. The measurements show theater_ratio rising from 0.12 to 0.28, suggesting that performative defense of the boundary is increasing relative to functional protection of genuine dissent. This is not conclusive evidence of mandatrophy, but it is suggestive: the extraction persists (extractiveness rises), while the founding problem remains contested (its status is 'contested' in the six_questions). If the founding problem were to shift decisively to 'dead' (i.e., if censorship of dissent were no longer a live threat in a particular jurisdiction), the mandatrophy analysis would need to reconsider whether the extraction is being maintained for its own sake rather than for the problem it was built to solve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_persistence,
    'Has the founding problem—governments and majorities using harm as a pretext for silencing dissent—remained a live threat, or has political and institutional evolution substantially reduced this danger in contemporary democracies?',
    'Comparative constitutional analysis over time: track instances of pretextual speech suppression (whether harm claims were genuinely about victim protection or were cover for political censorship), cross-sectional comparison of democracies with harm-based vs. absolutist speech regimes, and measurement of dissent protection in each regime.',
    'If the founding problem is judged ''dead'' in a particular jurisdiction, the mandatrophy analysis shifts: the extraction persists, but the justification dissolves. The constraint might then be reclassified as a piton (inertial doctrine) or snare (extraction justified by outdated threat). If the problem is judged ''live,'' the constraint retains its coordinative justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_persistence, empirical, 'Whether the historical threat animating the absolutist reading remains a genuine contemporary risk.').

omega_variable(
    harm_definition_ambiguity,
    'Is ''listener harm'' as rejected by the absolutist reading a coherent category, or does the reading rely on an artificially narrow definition that excludes genuine structural harms (harassment, subordination, identity erasure)?',
    'Philosophical analysis of harm definitions, empirical study of harm mechanisms in speech contexts (psychological, reputational, economic, group-subordinating), and assessment of whether narrow categorical exclusions (incitement, true threats, defamation) adequately capture the full spectrum of speech-mediated harm.',
    'If harm is found to be broader and more varied than the categorical exclusions permit, the absolutist reading may be reclassified as Tangled Rope or Snare (coordination function + substantial collateral extraction) rather than Rope. If the categorical exclusions are found to be sufficient, the reading''s Rope classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(harm_definition_ambiguity, conceptual, 'Whether the absolutist reading''s harm exclusion is sustainable or relies on category collapse.').

omega_variable(
    digital_scale_transformation,
    'Do scale effects from digital communication—algorithmic amplification, coordinated harassment, reach at near-infinite scale—materially transform the harm structure that the absolutist reading presumes, such that the protection boundary should shift?',
    'Comparative study of speech harm under print, broadcast, and digital regimes; measurement of whether digital-scale harassment meets the threshold for true threats or incitement in jurisdictions where absolutism is the governing doctrine; analysis of platform-mediated speech architecture.',
    'If digital scale is found to create qualitatively new harm mechanisms that the categorical exclusions do not accommodate, pressure will mount for the reading to be superseded by harm_threshold_reading or dignity_reading. If categorical exclusions are found to be sufficient, the absolutist reading persists despite scale transformation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_scale_transformation, empirical, 'Whether contemporary digital speech dynamics require revision of the protection boundary.').

omega_variable(
    reading_boundary_enforceability,
    'Is the absolutist reading''s bright-line boundary between protected and categorical exclusions actually bright, or does it require continuous contestation and interpretive judgment that collapses it functionally closer to harm-threshold territory?',
    'Analysis of case law: how many true-threat and incitement cases go to litigation, how often is the boundary revised, how much institutional energy is expended on boundary defense? Comparison with jurisdictions using harm-threshold readings: do they require less institutional maintenance or more?',
    'If the boundary is found to require high maintenance (rising theater_ratio, high suppression_requirement), the constraint''s classification may shift from Rope to Tangled Rope (coordination + substantial extraction). If the boundary is stable and low-maintenance, it is Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_enforceability, empirical, 'Whether the categorical exclusion boundary is stable or requires continuous institutional maintenance.').

omega_variable(
    speaker_diversity_empirical,
    'Does the absolutist reading''s protection of dissent actually protect a broad diversity of speakers (marginalized, subordinated, minority voices), or does it disproportionately protect already-powerful speakers while offering limited benefit to genuinely dissenting minorities?',
    'Empirical analysis: who benefits from absolutist speech protection (measure by demographic, institutional, and power position)? Who faces harm (same measures)? Does protection correlate with dissent-from-power or with speaker power independent of dissent status?',
    'If protection is found to be distributed unequally (benefiting the powerful disproportionately while marginalizing the vulnerable), the reading''s classification as Rope (genuine coordination) is undermined, and reclassification as Tangled Rope or Snare becomes warranted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speaker_diversity_empirical, empirical, 'Whether absolutist protection actually serves its stated function of protecting diverse dissent or concentrates benefits unequally.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__absolutist_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__absolutist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(spee_tr_t5, speech_protection_kernel__absolutist_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(spee_tr_t10, speech_protection_kernel__absolutist_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(spee_tr_t15, speech_protection_kernel__absolutist_reading, theater_ratio, 15, 0.25).
narrative_ontology:measurement(spee_tr_t20, speech_protection_kernel__absolutist_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(spee_tr_t25, speech_protection_kernel__absolutist_reading, theater_ratio, 25, 0.28).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__absolutist_reading, base_extractiveness, 0, 0.58).
narrative_ontology:measurement(spee_be_t5, speech_protection_kernel__absolutist_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(spee_be_t10, speech_protection_kernel__absolutist_reading, base_extractiveness, 10, 0.66).
narrative_ontology:measurement(spee_be_t15, speech_protection_kernel__absolutist_reading, base_extractiveness, 15, 0.69).
narrative_ontology:measurement(spee_be_t20, speech_protection_kernel__absolutist_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(spee_be_t25, speech_protection_kernel__absolutist_reading, base_extractiveness, 25, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__absolutist_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement(spee_su_t5, speech_protection_kernel__absolutist_reading, suppression_requirement, 5, 0.57).
narrative_ontology:measurement(spee_su_t10, speech_protection_kernel__absolutist_reading, suppression_requirement, 10, 0.59).
narrative_ontology:measurement(spee_su_t15, speech_protection_kernel__absolutist_reading, suppression_requirement, 15, 0.61).
narrative_ontology:measurement(spee_su_t20, speech_protection_kernel__absolutist_reading, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(spee_su_t25, speech_protection_kernel__absolutist_reading, suppression_requirement, 25, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__absolutist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_kernel__absolutist_reading, 0.12).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of five readings of the speech_protection_kernel. The absolutist reading (this file) maximizes speaker autonomy and rejects harm as grounds for restriction. Sibling readings condition or narrow protection based on harm (harm_threshold_reading), truth-discovery function (marketplace_reading), target dignity (dignity_reading), and democratic participation (democratic_participation_reading). All five readings share the same contested kernel but instantiate different ε values and victim/beneficiary structures. Links are bidirectional and bidirectional effects run through the engine's contamination protocol.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_kernel__absolutist_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
