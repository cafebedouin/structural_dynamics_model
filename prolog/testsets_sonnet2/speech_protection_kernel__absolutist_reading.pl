% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: speech_protection_kernel__absolutist_reading
 *   human_readable: Speech Protection Kernel — Absolutist (Near-Categorical) Reading
 *   domain: constitutional_law/political_philosophy
 *
 * SUMMARY:
 *   This story instantiates the absolutist reading of the speech-protection
 *   kernel: speech is protected near-categorically, and demonstrated listener
 *   or target harm is not, by itself, grounds for restriction unless the
 *   speech falls within narrow, independently-defined categorical exclusions
 *   (true threats, incitement to imminent lawless action, obscenity, certain
 *   defamation). This reading is generated as its own clean, ε-invariant
 *   constraint per the ε-invariance principle: the harm_threshold_reading,
 *   marketplace_reading, dignity_reading, and
 *   democratic_participation_reading are siblings living in separate
 *   constraint files, not alternative measurements of this one. This
 *   reading's ε is assessed by its own lights against the standing
 *   arrangement it defends (the current near-categorical doctrine), not
 *   against any sibling's endorsed alternative.
 *
 * KEY AGENTS:
 *   - controversial_speakers: primary beneficiary (moderate/mobile) — speaks without harm-based liability exposure
 *   - political_dissidents: primary beneficiary (powerless/constrained) — protection strongest where retaliation risk is highest
 *   - civil_liberties_litigators: agenda_setter (organized/arbitrage) — sets and defends the doctrinal boundary
 *   - targets_of_group_defamation, harassment_targets_in_public_forums, marginalized_group_members_subject_to_hate_speech: primary targets (powerless/trapped) — bear the cost the doctrine will not restrict
 *   - constitutional_courts: analytical observer (institutional/analytical) — adjudicates the categorical boundary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__absolutist_reading, 0.42).
domain_priors:suppression_score(speech_protection_kernel__absolutist_reading, 0.28).
domain_priors:theater_ratio(speech_protection_kernel__absolutist_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__absolutist_reading, rope).
narrative_ontology:human_readable(speech_protection_kernel__absolutist_reading, "Speech Protection Kernel — Absolutist (Near-Categorical) Reading").
narrative_ontology:topic_domain(speech_protection_kernel__absolutist_reading, "constitutional_law/political_philosophy").

domain_priors:requires_active_enforcement(speech_protection_kernel__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__absolutist_reading, '893f2192-4caa-4955-9467-54f075694a01').
narrative_ontology:cs_kernel_codification('893f2192-4caa-4955-9467-54f075694a01', fixed_text).
narrative_ontology:cs_authority_grounding('893f2192-4caa-4955-9467-54f075694a01', lineage).
narrative_ontology:cs_interpretation_layer_present('893f2192-4caa-4955-9467-54f075694a01').
narrative_ontology:cs_reading_relation('893f2192-4caa-4955-9467-54f075694a01', speech_protection_kernel__harm_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('893f2192-4caa-4955-9467-54f075694a01', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('893f2192-4caa-4955-9467-54f075694a01', speech_protection_kernel__dignity_reading, forecloses).
narrative_ontology:cs_reading_relation('893f2192-4caa-4955-9467-54f075694a01', speech_protection_kernel__democratic_participation_reading, influences).
narrative_ontology:cs_axiom('893f2192-4caa-4955-9467-54f075694a01', foundational, listener_harm_is_not_a_restriction_trigger).
narrative_ontology:cs_axiom_status(listener_harm_is_not_a_restriction_trigger, holdable).
narrative_ontology:cs_axiom_grounding('893f2192-4caa-4955-9467-54f075694a01', listener_harm_is_not_a_restriction_trigger, deontological).
narrative_ontology:cs_axiom('893f2192-4caa-4955-9467-54f075694a01', foundational, speaker_autonomy_is_categorically_prior_to_target_dignity_interests).
narrative_ontology:cs_axiom_status(speaker_autonomy_is_categorically_prior_to_target_dignity_interests, holdable).
narrative_ontology:cs_axiom_grounding('893f2192-4caa-4955-9467-54f075694a01', speaker_autonomy_is_categorically_prior_to_target_dignity_interests, deontological).
narrative_ontology:cs_reference_frame('893f2192-4caa-4955-9467-54f075694a01', dissident_protection_against_majoritarian_suppression).
narrative_ontology:cs_drift_state('893f2192-4caa-4955-9467-54f075694a01', contemporary_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('893f2192-4caa-4955-9467-54f075694a01', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__absolutist_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, controversial_speakers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, political_dissidents).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, media_publishers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, civil_liberties_litigators).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, targets_of_group_defamation).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, harassment_targets_in_public_forums).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, marginalized_group_members_subject_to_hate_speech).
narrative_ontology:constraint_vindicates(speech_protection_kernel__absolutist_reading, speaker_autonomy_primacy_doctrine).
narrative_ontology:constraint_vindicates(speech_protection_kernel__absolutist_reading, anti_paternalism_in_expression_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Say things that provoke, offend, or advocate unpopular positions and are shielded from civil or criminal liability so long as the speech does not cross narrow categorical exclusions (true threats, incitement to imminent lawless action, obscenity, defamation with actual malice). They can speak freely without pre-clearance and face no listener-harm-based restriction.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, controversial_speakers, beneficiary,
    moderate, biographical, mobile, national).

% Depend on the near-categorical rule to criticize government and powerful institutions without the risk that officials could recharacterize their speech as harmful and suppress it. Their protection is strongest precisely where their power to resist retaliation through other channels is weakest.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, political_dissidents, beneficiary,
    powerless, generational, constrained, national).

% Publish investigative and opinion content, including material that offends or upsets identifiable people or groups, relying on the doctrine's refusal to weigh listener distress as an independent basis for liability.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, media_publishers, beneficiary,
    organized, generational, mobile, national).

% Bring and win the test cases that establish and defend the near-categorical boundary, argue against harm-based carve-outs in court and in public discourse, and set the doctrinal agenda that other seats must operate within. They do not personally speak the protected speech but administer and extend the rule.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, civil_liberties_litigators, agenda_setter,
    organized, civilizational, arbitrage, national).

% Are the subject of defamatory or degrading speech directed at their group identity; because the doctrine treats listener/target harm as insufficient grounds for restriction absent a narrow categorical trigger, they have no legal recourse for reputational and dignitary injury short of proving the speech falls into an excluded category. They cannot exit the public sphere where the speech circulates.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, targets_of_group_defamation, payer,
    powerless, biographical, trapped, local).

% Experience sustained hostile speech in workplaces, campuses, or online platforms that stops short of a true threat or incitement; the near-categorical rule denies them a harm-based claim, forcing them either to endure the exposure or to exit the forum themselves — the doctrine allocates the cost of the speech's circulation onto them rather than the speaker.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, harassment_targets_in_public_forums, payer,
    powerless, immediate, trapped, local).

% Bear the cumulative, society-wide cost of speech that degrades their standing as equal participants, without the doctrine recognizing dignitary or subordinating effects as restriction-worthy harm; their only recourse is counter-speech, which requires resources and platforms they frequently lack.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, marginalized_group_members_subject_to_hate_speech, payer,
    powerless, generational, trapped, national).

% Would prefer to weigh demonstrable listener and community harm as one factor among several in moderation policy, but the absolutist reading's dominance in constitutional and platform-governance discourse forecloses harm-based frameworks from being treated as legally or normatively primary; their harm-weighing proposals are treated as illegitimate departures from the baseline.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, content_moderation_bodies, excluded,
    institutional, biographical, constrained, global).

% Adjudicate where the categorical exclusions lie, hear challenges from both speakers and targets, and can in principle narrow or widen the boundary through case law, though under this reading they treat the near-categorical baseline as the default requiring strong justification to depart from.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, constitutional_courts, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable, content-neutral rule that prevents government and majorities from suppressing dissenting, unpopular, or offensive speech under a manipulable harm standard — coordinating expectations so speakers need not guess whether their speech will later be deemed harmful enough to punish.
% TRANSFER_FUNCTION: Moves the cost of tolerating offensive, degrading, or dignitary-injurious speech from the speaker and the legal system onto the individuals and groups targeted by that speech, who absorb the reputational, psychological, and social costs without a corresponding restriction remedy.
% ABSENT_VOICES: Targets of group defamation and hate speech, and the institutions (content moderation bodies, some civil rights litigators) that would weigh listener harm, are structurally excluded from setting the doctrinal baseline; they participate only as challengers to a rule already set against harm-based claims, and their objections are treated as attempts to erode a settled boundary rather than as founding input.
% DISAPPEARANCE_RATIONALE: If the near-categorical rule vanished overnight, speech regulation would shift toward case-by-case harm balancing: many currently-protected but offensive or degrading statements would become actionable, political and media speakers would face materially higher legal uncertainty and self-censorship risk, and government and private plaintiffs would gain a new lever to suppress unpopular expression — the entire landscape of what can be said without liability would reorganize.
% FOUNDING_PROBLEM: Historical experience with governments and majorities using vague harm, decency, or public-order standards to criminalize dissent, minority religious expression, and unpopular political speech — the founding problem was the manipulability and chilling effect of harm-based or offense-based speech restrictions in the hands of the powerful.
% FOUNDING_PROBLEM_CORROBORATION: Civil liberties litigators and constitutional courts attest the founding problem (majoritarian suppression of dissent) remains live, citing ongoing prosecutions of dissidents in other jurisdictions and periodic domestic attempts to regulate offensive political speech. Civil rights scholars and dignitary-harm litigators, outside the beneficiary set, attest that the doctrine's persistence in its near-categorical form has outrun the founding problem and now functions primarily to shield organized hate speech and harassment campaigns that the historical dissent-protection rationale was never meant to cover; this is a genuinely contested genealogy, not settled by either side alone.
narrative_ontology:disappearance_verdict(speech_protection_kernel__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_kernel__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__absolutist_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__absolutist_reading_tests).
:- end_tests(speech_protection_kernel__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at a moderate 0.42, rising slowly over the interval, reflecting a genuine and substantial coordination function (protecting dissent and unpopular political speech from majoritarian suppression) layered with a real, non-trivial cost imposed on identifiable target groups whose dignitary and reputational harms go unremedied. This is not scored as high-extraction snare-level because the coordination function is real and the doctrine's core justification (protecting dissenters from a manipulable harm standard) remains partially live. Suppression is authored lower (0.28) than extraction because the doctrine's mechanism is largely permissive (it prevents restriction) rather than actively coercive against any party except insofar as courts strike down harm-based remedies sought by targets — the 'suppression' here is the suppression of alternative harm-based frameworks from gaining doctrinal traction, not direct coercion of speakers. Accessibility collapse is moderate (0.35): harm-based alternatives are argued and litigated continuously, they are not eliminated, merely disfavored as a matter of doctrinal default. Resistance is comparatively high (0.6) because dignity- and harm-based advocates mount continuous, well-organized doctrinal and political challenges to the near-categorical baseline.
 *
 * DIRECTIONALITY LOGIC:
 *   Controversial speakers, political dissidents, media publishers, and civil liberties litigators sit near the beneficiary end: the doctrine subsidizes their expressive activity by removing harm-based liability risk. Targets of group defamation, harassment targets, and marginalized group members subject to hate speech sit near the target end: they bear costs the doctrine will not let them convert into a remedy, and their exit options are trapped (they cannot exit the public sphere or the social environment in which the speech circulates). Political dissidents are an important asymmetry within the beneficiary class: despite being powerless in general political terms, they are structural beneficiaries of THIS specific doctrine because it is designed to protect exactly their position against a government that would weaponize a harm standard against them — this is why their exit_options is 'constrained' rather than 'mobile' even though their directionality toward the constraint itself is protective.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (protecting dissidents and unpopular speakers from manipulable, majoritarian harm-based suppression) remains partially live — authoritarian and majoritarian suppression of dissent is an ongoing global phenomenon, which argues against a simple 'dead mandate' verdict. But the doctrine's near-categorical breadth extends well past dissident protection to shield organized, non-political, target-specific degradation campaigns that the founding rationale was never designed to cover. The founding_problem_status is authored as 'contested' rather than 'dead' precisely to avoid mislabeling a doctrine with a partially-live coordination function as pure extraction — the mismatch consumer should read (status=contested + verdict=world_rearranges) as flagging a genealogy dispute worth investigating, not as a capture verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dissident_protection_vs_hate_speech_shield,
    'Does the near-categorical rule''s persistence still track its founding function (protecting dissidents from majoritarian harm-based suppression), or has it drifted into primarily shielding organized group-targeted degradation that the founding rationale never contemplated?',
    'Comparative case-law analysis of which fact patterns actually invoke and benefit from the near-categorical baseline over time: political dissent cases versus group-defamation/harassment cases, tracked longitudinally.',
    'If dissident-protection invocations are declining relative to hate-speech-shielding invocations, the doctrine''s coordination function has substantially atrophied even though its formal justification has not changed — supporting a piton-adjacent reading despite the ''rope'' claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dissident_protection_vs_hate_speech_shield, empirical, 'Whether the doctrine''s actual caseload still matches its founding dissident-protection rationale.').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (absolutist_reading) of the speech_protection_kernel. The sibling readings — harm_threshold_reading, marketplace_reading, dignity_reading, democratic_participation_reading — are separate constraints with different beneficiary/victim structures and different ε. Where exactly is the disagreement located structurally?',
    'The disagreement is located specifically at the threshold question: does demonstrated listener/target harm count as an independent, sufficient trigger for restriction? This reading answers no (only narrow categorical exclusions count); harm_threshold_reading answers yes; dignity_reading answers conditionally (only subordinating harm counts); marketplace_reading and democratic_participation_reading relocate the question entirely (truth-discovery function and political-speech centrality, respectively, rather than a harm threshold at all).',
    'A court or platform adopting harm_threshold_reading or dignity_reading instead of this reading would grant standing to exactly the payer stakeholders this story names as trapped/powerless (targets_of_group_defamation, harassment_targets_in_public_forums, marginalized_group_members_subject_to_hate_speech) — their victim status is a direct artifact of which reading is adopted, not an independent empirical fact.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Locates the structural disagreement among kernel siblings at the harm-as-restriction-trigger threshold.').

omega_variable(
    categorical_exclusion_boundary_stability,
    'Are the ''narrow categorical exclusions'' (true threats, incitement, obscenity, defamation) themselves stable and non-manipulable, or do they function as a pressure valve that silently expands or contracts to absorb political pressure while preserving the appearance of a fixed near-categorical rule?',
    'Track how frequently and in which political directions courts redraw the boundaries of ''true threat'' and ''incitement'' over the measurement interval; a boundary that moves opportunistically undermines the claim that the rule is genuinely categorical rather than harm-balancing in disguise.',
    'If the exclusion categories are themselves elastic and outcome-driven, the ''near-categorical'' framing is partly theatrical — the doctrine performs bright-line certainty while actually conducting the harm balancing it claims to reject, which would raise the honestly-authored theater_ratio.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(categorical_exclusion_boundary_stability, conceptual, 'Whether the categorical exclusions are genuinely fixed or a disguised balancing mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__absolutist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__absolutist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(spee_tr_t12, speech_protection_kernel__absolutist_reading, theater_ratio, 12, 0.12).
narrative_ontology:measurement(spee_tr_t24, speech_protection_kernel__absolutist_reading, theater_ratio, 24, 0.14).
narrative_ontology:measurement(spee_tr_t36, speech_protection_kernel__absolutist_reading, theater_ratio, 36, 0.16).
narrative_ontology:measurement(spee_tr_t48, speech_protection_kernel__absolutist_reading, theater_ratio, 48, 0.18).
narrative_ontology:measurement(spee_tr_t60, speech_protection_kernel__absolutist_reading, theater_ratio, 60, 0.2).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__absolutist_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(spee_be_t12, speech_protection_kernel__absolutist_reading, base_extractiveness, 12, 0.27).
narrative_ontology:measurement(spee_be_t24, speech_protection_kernel__absolutist_reading, base_extractiveness, 24, 0.33).
narrative_ontology:measurement(spee_be_t36, speech_protection_kernel__absolutist_reading, base_extractiveness, 36, 0.37).
narrative_ontology:measurement(spee_be_t48, speech_protection_kernel__absolutist_reading, base_extractiveness, 48, 0.4).
narrative_ontology:measurement(spee_be_t60, speech_protection_kernel__absolutist_reading, base_extractiveness, 60, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__absolutist_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(spee_su_t12, speech_protection_kernel__absolutist_reading, suppression_requirement, 12, 0.23).
narrative_ontology:measurement(spee_su_t24, speech_protection_kernel__absolutist_reading, suppression_requirement, 24, 0.24).
narrative_ontology:measurement(spee_su_t36, speech_protection_kernel__absolutist_reading, suppression_requirement, 36, 0.26).
narrative_ontology:measurement(spee_su_t48, speech_protection_kernel__absolutist_reading, suppression_requirement, 48, 0.27).
narrative_ontology:measurement(spee_su_t60, speech_protection_kernel__absolutist_reading, suppression_requirement, 60, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__absolutist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_kernel__absolutist_reading, 0.1).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, democratic_participation_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling constraints decomposing the natural-language concept 'speech protection kernel' per the ε-invariance principle. Each reading (absolutist, harm_threshold, marketplace, dignity, democratic_participation) is authored as its own constraint with its own ε, beneficiary/victim structure, and claimed type, because the readings differ on where the restriction threshold sits and thus produce structurally different extraction and victim profiles. All five are linked via network.affects_constraints to preserve the family relationship; none should be read as an alternative measurement of a single shared ε.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
