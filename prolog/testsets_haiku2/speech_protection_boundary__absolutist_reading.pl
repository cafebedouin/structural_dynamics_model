% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_boundary__absolutist_reading, []).

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
 *   constraint_id: speech_protection_boundary__absolutist_reading
 *   human_readable: Absolutist Speech Protection (Brandenburg Standard)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The absolutist reading of the speech protection boundary holds that First
 *   Amendment protection is near-absolute; speech receives the state's
 *   protection unless it constitutes direct incitement to imminent lawless
 *   action (Brandenburg v. Ohio standard). The consequence of this reading,
 *   when viewed through the lens of minoritized groups, is that hostile
 *   speech targeting them falls outside legal regulation even when cumulative
 *   and damaging. The constraint is CLAIMED as tangled_rope (it coordinates
 *   state power restraint while extracting from targets) and the metrics
 *   reflect substantial asymmetry: high extractiveness because benefits flow
 *   to speakers and institutional media while costs fall on targets; moderate
 *   suppression because the constraint's operation depends on courts actively
 *   and repeatedly defending the Brandenburg boundary; moderate theater
 *   because while real adjudication occurs, the justificatory narrative
 *   (protecting political dissent) increasingly covers majoritarian and
 *   institutional speech.
 *
 * KEY AGENTS:
 *   - speakers_with_majority_position: Benefit from expansive protection of their expression; enjoy arbitrage exit (they can voluntarily restrain but are never legally required to)
 *   - institutional_speakers_and_media: Media outlets, universities, state agencies operating with legal certainty; mobile exit (can adopt higher standards voluntarily)
 *   - target_communities_of_speech: Bear hostile speech, dehumanization, harassment; identity-locked into their status as targets (cannot exit the characteristic by which they are targeted)
 *   - minoritized_groups_facing_cumulative_harassment: Aggregate harm from coordinated protected hostile speech; constrained exit (can migrate platforms or communities but not address legal immunity of speech)
 *   - courts_interpreting_brandenburg: Agenda-setters administering the boundary through case law; police the bright line
 *   - legislatures_and_regulators: Excluded from regulating speech meeting Brandenburg's threshold; foreclosed by constitutional doctrine
 *   - harm_advocates_and_speech_restrictions_movement: Excluded; their regulatory proposals fail or are never attempted under Brandenburg
 *   - comparative_legal_jurisdictions: Observer; alternative regulatory frameworks visible but structurally unavailable
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__absolutist_reading, 0.68).
domain_priors:suppression_score(speech_protection_boundary__absolutist_reading, 0.41).
domain_priors:theater_ratio(speech_protection_boundary__absolutist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, suppression_requirement, 0.41).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__absolutist_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_boundary__absolutist_reading, "Absolutist Speech Protection (Brandenburg Standard)").
narrative_ontology:topic_domain(speech_protection_boundary__absolutist_reading, "constitutional/political").

domain_priors:requires_active_enforcement(speech_protection_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__absolutist_reading, 'e907233d-59ed-4492-90e2-eb6264f6e5f7').
narrative_ontology:cs_kernel_codification('e907233d-59ed-4492-90e2-eb6264f6e5f7', fixed_text).
narrative_ontology:cs_authority_grounding('e907233d-59ed-4492-90e2-eb6264f6e5f7', lineage).
narrative_ontology:cs_interpretation_layer_present('e907233d-59ed-4492-90e2-eb6264f6e5f7').
narrative_ontology:cs_reading_relation('e907233d-59ed-4492-90e2-eb6264f6e5f7', speech_protection_boundary__harm_limited_reading, coexists_with).
narrative_ontology:cs_reading_relation('e907233d-59ed-4492-90e2-eb6264f6e5f7', speech_protection_boundary__balancing_reading, coexists_with).
narrative_ontology:cs_axiom('e907233d-59ed-4492-90e2-eb6264f6e5f7', foundational, state_incompetent_to_judge_speech_harm).
narrative_ontology:cs_axiom_status(state_incompetent_to_judge_speech_harm, holdable).
narrative_ontology:cs_axiom_grounding('e907233d-59ed-4492-90e2-eb6264f6e5f7', state_incompetent_to_judge_speech_harm, deontological).
narrative_ontology:cs_axiom('e907233d-59ed-4492-90e2-eb6264f6e5f7', foundational, bright_line_protection_prevents_suppression_creep).
narrative_ontology:cs_axiom_status(bright_line_protection_prevents_suppression_creep, holdable).
narrative_ontology:cs_axiom_grounding('e907233d-59ed-4492-90e2-eb6264f6e5f7', bright_line_protection_prevents_suppression_creep, instrumental).
narrative_ontology:cs_reference_frame('e907233d-59ed-4492-90e2-eb6264f6e5f7', speech_protection_via_state_restraint).
narrative_ontology:cs_drift_state('e907233d-59ed-4492-90e2-eb6264f6e5f7', contemporary_2025, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e907233d-59ed-4492-90e2-eb6264f6e5f7', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__absolutist_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, speakers_with_majority_position).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, institutional_speakers_and_media).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, target_communities_of_speech).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, minoritized_groups_facing_cumulative_harassment).
narrative_ontology:constraint_vindicates(speech_protection_boundary__absolutist_reading, first_amendment_supremacy_over_competing_interests).
narrative_ontology:constraint_vindicates(speech_protection_boundary__absolutist_reading, state_incompetence_to_judge_harmful_speech).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enjoy expansive speech protection under Brandenburg: they can advocate positions hostile to minoritized groups, use dehumanizing language about target communities, and organize politically around exclusionary platforms without legal liability as long as they do not explicitly call for imminent violence. They benefit from the constraint's near-absolute protection of their political expression. Exit: they could request content moderation or self-regulate, but the constraint guarantees legal immunity regardless.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, speakers_with_majority_position, beneficiary,
    organized, biographical, arbitrage, national).

% Media institutions and institutional voices (universities, corporations, state agencies) operate with legal certainty under Brandenburg. They can publish, broadcast, or disseminate speech that shapes public discourse about minoritized groups with minimal legal exposure. The constraint protects their editorial independence and their ability to reach large audiences with unfiltered content. Exit: they could adopt higher editorial standards voluntarily, but the constraint does not require it.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, institutional_speakers_and_media, beneficiary,
    powerful, generational, mobile, national).

% Bear targeted hostile speech, dehumanizing narratives, and repeated public vilification without recourse to speech law remedies. They cannot sue for harassment that falls short of direct incitement, cannot restrain speech that portrays them as threats or subhuman, and must absorb the psychic and reputational costs. Their identity (the characteristic by which they are targeted—race, religion, gender identity, sexual orientation) is precisely what they cannot exit. They navigate public space knowing they are subjects of expansive protected hostile speech.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, target_communities_of_speech, payer,
    powerless, biographical, identity_locked, national).

% Face the aggregate effect of protected hostile speech: coordinated harassment campaigns, conspiracy theories, dehumanizing memes and narratives, all shielded by Brandenburg protection. While no single utterance crosses into direct incitement, the cumulative effect shapes threat environment, social exclusion, and material harm (online harassment leading to doxxing, offline violence by radicalized individuals emboldened by the speech ecosystem). They have limited exit: they can leave online platforms, migrate communities, or change visibility, but none addresses the legal immunity of the speech itself.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, minoritized_groups_facing_cumulative_harassment, payer,
    moderate, biographical, constrained, national).

% Administer the Brandenburg standard through case law, setting the threshold for what counts as imminent lawless action and policing the boundary between protected and unprotected speech. They enforce the constraint by rejecting speech liability claims that allege harm short of direct incitement. They maintain the bright-line rule even when faced with evidence of cumulative harassment or extremism radicalization.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, courts_interpreting_brandenburg, agenda_setter,
    institutional, generational, analytical, national).

% Are largely foreclosed from regulating speech that does not meet the Brandenburg threshold, even when constituent groups report significant harm. They cannot criminalize hate speech, cannot impose civil liability for harassment or incitement to violence short of directness, cannot mandate content moderation standards that restrict speech protected under Brandenburg. Their policy options are confined to narrow channels.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, legislatures_and_regulators, excluded,
    institutional, generational, trapped, national).

% Argue that Brandenburg protection tolerates unacceptable harm to minoritized communities, that cumulative harassment and incitement-adjacent speech should be regulated, and that competing constitutional values (equal protection, freedom from assault, dignity) should constrain speech. They remain outside the constitutional framework: their arguments have not prevailed; Brandenburg remains the operative standard; their proposed regulations are struck down or never attempted.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, harm_advocates_and_speech_restrictions_movement, excluded,
    moderate, biographical, constrained, national).

% Provide comparative contrast: Canada, UK, Germany, and other liberal democracies operate under speech standards that criminalize hate speech, incitement to violence beyond direct imminent action, or harassment—and report different outcomes for minoritized communities. Their regulatory frameworks are unavailable in the U.S. constitutional context but visible as alternatives.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, comparative_legal_jurisdictions, observer,
    powerful, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_boundary__absolutist_reading, institutional_speakers_and_media).
narrative_ontology:fixing_cost_class(speech_protection_boundary__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a predictable, judicially administrable boundary for state suppression of speech: speech receives maximum protection by default; the state may restrict only direct incitement to imminent lawless action. This solves the coordination problem of preventing the state from using 'harm' or 'offense' as a pretext for suppressing political dissent, journalism, or unpopular expression.
% TRANSFER_FUNCTION: Transfers the cost of speech harm from speakers and their audiences to target communities: speakers are shielded from legal liability; targets absorb the cost of hostile speech, harassment, and the discursive marginalization it enables. The arrangement moves legal immunity to speakers and reputational/psychic/material harm to targets.
% ABSENT_VOICES: Minoritized communities bearing cumulative harassment are structurally under-represented in constitutional speech doctrine: they can testify to harm but cannot prevail on speech liability claims; their experience of cumulative hostile speech environment is rendered legally invisible by the Brandenburg threshold; they are excluded from the constitutional conversation about what counts as speech harm worth regulating.
% DISAPPEARANCE_RATIONALE: Absolutist reading: If Brandenburg protection were removed and replaced with balancing or harm-focused standards, the constitutional system would reorganize around competing values (equality, dignity, freedom from harassment) and the state would gain regulatory power over speech—which would create novel suppression risks. Harm-focused reading: If Brandenburg were removed and speech law recognized significant harm to dignitary and equality interests, minoritized communities would gain legal remedies, speech ecosystem would be restructured around accountability, and power over speech would shift. The disappearance verdict is contested because whether the world 'rearranges' depends on whether one treats the Brandenburg boundary as protecting vital political freedom or enabling cumulative harassment.
% FOUNDING_PROBLEM: The founding problem was preventing state suppression of political dissent and unpopular speech through abuse of vague 'harm' standards. The absolutist reading was built to constrain state power: prior to Brandenburg (1960s), states and federal courts used 'seditious libel' and 'breach of peace' doctrines to criminalize civil rights advocacy, antiwar speech, and radical political organizing. Brandenburg was designed to protect the civil rights movement and political minorities from state suppression.
% FOUNDING_PROBLEM_CORROBORATION: Absolutist seat attests the founding problem remains live: the state still has incentives to suppress dissent; regulatory alternatives to Brandenburg would enable suppression. Harm advocates and minoritized groups attest the founding problem is substantially solved and Brandenburg now protects harmful speech rather than dissent: the primary speech-suppression vectors today operate through private harassment, algorithmic amplification, and corporate content moderation—not state prosecution. Comparative legal scholars attest that robust speech protection without Brandenburg's absolutism is maintained in peer democracies without systematic state suppression (Canada, UK, Germany enforce hate speech law and maintain press freedom and political dissent). Historical analysis from outside the benefiting parties shows Brandenburg emerged specifically to protect civil rights advocacy and has since become a shield for majority-position speakers and institutional media.
narrative_ontology:disappearance_verdict(speech_protection_boundary__absolutist_reading, contested).
narrative_ontology:founding_problem_status(speech_protection_boundary__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(speech_protection_boundary__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__absolutist_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_protection_boundary__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness of 0.68 reflects that the constraint redistributes legal immunity to speakers and liability/harm to targets. The beneficiary side (speakers, institutional media) collects legal protection and audiences; the payer side (targets) absorbs hostile speech environment and restricted legal remedies. Suppression of 0.41 is moderate because Brandenburg's boundary is actively maintained and contested: courts must regularly defend it against challenges, legislatures attempt workarounds, harm advocates mount doctrinal critiques. The suppression is not high because the boundary is stable and widely accepted within U.S. constitutional culture—it is not necessary to deploy heavy coercion; the doctrinal consensus holds it. Theater ratio of 0.28 reflects that Brandenburg's stated justification (protecting political dissent and preventing state suppression) was once its actual function (1960s-1970s civil rights era) but now increasingly covers majoritarian and institutional speech; the performative element (citing civil rights protection while permitting majority-position hostile speech) has grown as the internet enabled coordinated harassment that did not exist when Brandenburg was formulated. Accessibility collapse of 0.72 reflects that alternatives to absolute protection are difficult for speakers to access: they can make unilateral choices to restrain, but the legal framework does not require restraint, so the default accessibility to unrestricted speech is high. Resistance of 0.78 reflects persistent and organized contestation from harm advocates, minoritized communities, and some legislative bodies (and global comparison showing working alternatives).
 *
 * PERSPECTIVAL GAP:
 *   The absolutist seat (courts, First Amendment maximalists, institutional media) experiences Brandenburg as protecting vital political freedom and minority dissent—they emphasize the founding problem (state suppression) and its continued relevance, read target communities' harms as externalities not directly caused by speech protection itself (but by speakers' choices), and see alternatives as regulatory capture risks. The target-community seat experiences Brandenburg as legal immunity for hostile speech targeting them, asymmetric protection (speakers are protected; targets are not), and cumulative harm that is legally invisible because no single utterance meets the Brandenburg threshold. The harm-advocate seat experiences Brandenburg as a constitutional fiction: the founding problem (state suppression of political dissent) is substantially solved and Brandenburg now protects majority and institutional speech while blocking regulations that would address coordinated harassment and hate speech. The engine computes these divergences from the structural data: beneficiary/victim declarations, exit options (arbitrage vs. identity-locked), and power asymmetries. The claim and metrics are authored independently: the reading claims tangled rope (genuine coordination function + asymmetric extraction), and the metrics describe high extractiveness and moderate suppression that align with that claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Speakers with majority position have d near 0.0 (full beneficiary): they benefit from legal protection, have arbitrage exit (can choose to restrain but are never required), and sit in organized power positions. Institutional speakers have d near 0.1 (beneficiary, slightly higher than individuals because they wield greater reach and face more organized resistance). Target communities have d near 0.95 (near-full target): they bear cumulative harm, cannot exit their identity, sit in powerless positions, and have no legal remedy under Brandenburg. Minoritized groups facing harassment have d near 0.85 (target): they suffer aggregate harm, have constrained exit (they can change online presence but not exit the targeting itself), and sit in moderate power positions organizationally. Courts have d near 0.5 (symmetric): they are institutional agents who maintain the boundary but do not directly collect or pay; they are subject to constitutional authority themselves and face pressure from both beneficiary and harm-advocate seats. No overrides are necessary: the structural derivation from beneficiary/victim + exit + power produces accurate directionalities.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's founding problem was preventing state suppression of political dissent through abuse of vague harm standards—Brandenburg was built to protect civil rights advocacy and antiwar speech from prosecution. This founding problem WAS live and urgent in the 1960s-1970s; Brandenburg solved it by establishing a bright-line rule that would prevent courts from using 'offense' or 'breach of peace' to suppress minority viewpoints. The founding problem status is NOW contested: the absolutist reading maintains that state suppression risks remain and Brandenburg is necessary prophylaxis; harm advocates and minoritized communities attest that the primary suppression vectors have shifted to private harassment and algorithmic amplification, not state prosecution, and that Brandenburg now protects those vectors. The disappearance verdict is contested: absolutists believe removal of Brandenburg would enable state suppression; harm advocates believe removal would enable regulations that are widespread in peer democracies without systematic suppression. This mismatch (contested status x contested verdict) is the signature of mandatrophy: the constraint's mandate is obsolete for its original function (preventing state suppression of political dissent) and has been repurposed to protect majority-position speech from accountability. The theater_ratio trajectory (rising from 0.12 to 0.28) documents this: Brandenburg's protective justification (preventing state suppression) is increasingly performative as applied to institutional and majority-position speakers, while its actual function is enabling hostile speech immunity. The constraint is a Tangled Rope because it has BOTH a real coordination function (restraining state power, protecting political dissent as a live value) AND asymmetric extraction (legal immunity accruing to speakers, harm concentrated on targets). The asymmetry persists through active enforcement: courts must repeatedly police the Brandenburg boundary to prevent expansion of unprotected categories.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_obsolescence_vs_persistence,
    'Is the founding problem (state suppression of political dissent through vague harm standards) substantially solved, or does it remain a live constitutional risk?',
    'Empirical mapping of speech-suppression vectors: (1) state prosecution under Brandenburg-type standards (measure frequency, targeting patterns), (2) alternative regulatory pathways legislatures attempt (assess whether they function as content suppression or speech protection). Historical analysis of whether Brandenburg''s protective function was necessary for civil rights protection and is still necessary for political minorities today.',
    'If the founding problem is substantially solved, Brandenburg persists as extract-enabling theater—the constraint is a degraded scaffold or a piton. If the founding problem remains live and serious, Brandenburg remains a vital coordination function and the extracted harm is a necessary price. The classification branches on this: Piton vs. Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(founding_problem_obsolescence_vs_persistence, empirical, 'Whether Brandenburg''s original protective mission is still necessary or has become a cover story for immunity to majority-position speech.').

omega_variable(
    cumulative_harm_vs_individual_utterance_boundary,
    'Is the harm to minoritized communities from coordinated hostile speech fundamentally different in kind from the harm addressed by Brandenburg (direct incitement), or is it the same phenomenon measured at a different scale?',
    'Comparative legal and empirical analysis of jurisdictions with hate speech law: do they experience different harm outcomes for minoritized groups while maintaining press freedom and political dissent protection? Psychological and social research on cumulative versus acute harassment effects and their relationship to Brandenburg''s imminent-lawless-action threshold.',
    'If cumulative harm is structurally different and requires different legal responses, Brandenburg''s one-threshold approach misclassifies the harm and leaves targets unprotected. The constraint''s classification would shift toward Snare. If cumulative harm is an artifact of scale rather than kind, Brandenburg''s bright-line remains defensible and the classification as Tangled Rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cumulative_harm_vs_individual_utterance_boundary, empirical, 'Whether Brandenburg''s focus on acute incitement captures or misses the structural harm to minoritized groups through cumulative coordinated hostile speech.').

omega_variable(
    alternative_reading_framings_within_absolutism,
    'Could an absolutist reading of Brandenburg coexist with stronger harm remedies for minoritized communities (e.g., through civil harassment law, organizational liability, social platform regulation) WITHOUT compromising the political-dissent-protection core?',
    'Comparative legal analysis of jurisdictions that maintain speech protection for political expression and journalism while regulating hate speech or organized harassment. Doctrinal analysis of whether civil harassment remedies are compatible with Brandenburg or logically foreclose it.',
    'If compatible, the absolutist reading and harm-focused reading are not logically opposed but rather represent different institutional choices. If incompatible, they are genuine alternatives competing for constitutional authority. The reading_relations classification depends on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_framings_within_absolutism, conceptual, 'Whether absolutist and harm-focused readings are logically foreclosed or merely represent different policy choices.').

omega_variable(
    identity_lock_duration_in_targets,
    'Is the identity-locked status of target communities a permanent feature of how marginalization operates, or a contingent institutional effect that could be loosened through remedial law?',
    'Examine whether access to legal remedies and social recognition change how identity functions as a lock. Compare before/after contexts where harm remedies were introduced (jurisdictions that adopted hate speech law or institutional accountability). Assess whether targets'' sense of exit options shifted when speech law changed.',
    'If identity-lock is permanent, the constraint''s asymmetry is inherent to the target communities'' structural position. If identity-lock is contingent on lack of remedy, expanding legal protection loosens the lock and shifts target d values downward, reducing effective extraction. This affects whether the constraint can be reformed or must be replaced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_duration_in_targets, empirical, 'Whether target communities'' identity-locked exit status is a feature of marginalization itself or a consequence of Brandenburg immunity.').

omega_variable(
    reading_incompatibility_foreclosure_test,
    'Do the absolutist and harm-limited readings logically foreclose one another—i.e., could no framework coherently hold both—or do they merely represent different parties'' preferences?',
    'Doctrinal analysis of whether absolute speech protection is logically incompatible with harm prevention, or whether the incompatibility is institutional (what the U.S. constitutional framework adopted) rather than logical. Examine whether a hypothetical system could protect political dissent and journalism while regulating cumulative hostile speech—do such systems exist in peer democracies?',
    'If readings logically foreclose each other, reading_relations = forecloses. If they can coexist in different institutional contexts or through creative doctrinal accommodation, reading_relations = coexists_with or influences. This determines whether the kernel is genuinely contested (both readings live) or one rules out the other.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_incompatibility_foreclosure_test, conceptual, 'Whether absolutist and harm-limited readings are logically incompatible or merely institutionally opposed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__absolutist_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(speech_absolutist_tr_t1970, speech_protection_boundary__absolutist_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement_basis(speech_absolutist_tr_t1970, observed).
narrative_ontology:measurement(speech_absolutist_tr_t1985, speech_protection_boundary__absolutist_reading, theater_ratio, 1985, 0.16).
narrative_ontology:measurement_basis(speech_absolutist_tr_t1985, observed).
narrative_ontology:measurement(speech_absolutist_tr_t2000, speech_protection_boundary__absolutist_reading, theater_ratio, 2000, 0.2).
narrative_ontology:measurement_basis(speech_absolutist_tr_t2000, observed).
narrative_ontology:measurement(speech_absolutist_tr_t2010, speech_protection_boundary__absolutist_reading, theater_ratio, 2010, 0.24).
narrative_ontology:measurement_basis(speech_absolutist_tr_t2010, observed).
narrative_ontology:measurement(speech_absolutist_tr_t2018, speech_protection_boundary__absolutist_reading, theater_ratio, 2018, 0.27).
narrative_ontology:measurement_basis(speech_absolutist_tr_t2018, observed).
narrative_ontology:measurement(speech_absolutist_tr_t2025, speech_protection_boundary__absolutist_reading, theater_ratio, 2025, 0.28).
narrative_ontology:measurement_basis(speech_absolutist_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(speech_absolutist_be_t1970, speech_protection_boundary__absolutist_reading, base_extractiveness, 1970, 0.42).
narrative_ontology:measurement_basis(speech_absolutist_be_t1970, observed).
narrative_ontology:measurement(speech_absolutist_be_t1985, speech_protection_boundary__absolutist_reading, base_extractiveness, 1985, 0.51).
narrative_ontology:measurement_basis(speech_absolutist_be_t1985, observed).
narrative_ontology:measurement(speech_absolutist_be_t2000, speech_protection_boundary__absolutist_reading, base_extractiveness, 2000, 0.59).
narrative_ontology:measurement_basis(speech_absolutist_be_t2000, observed).
narrative_ontology:measurement(speech_absolutist_be_t2010, speech_protection_boundary__absolutist_reading, base_extractiveness, 2010, 0.64).
narrative_ontology:measurement_basis(speech_absolutist_be_t2010, observed).
narrative_ontology:measurement(speech_absolutist_be_t2018, speech_protection_boundary__absolutist_reading, base_extractiveness, 2018, 0.67).
narrative_ontology:measurement_basis(speech_absolutist_be_t2018, observed).
narrative_ontology:measurement(speech_absolutist_be_t2025, speech_protection_boundary__absolutist_reading, base_extractiveness, 2025, 0.68).
narrative_ontology:measurement_basis(speech_absolutist_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(speech_absolutist_su_t1970, speech_protection_boundary__absolutist_reading, suppression_requirement, 1970, 0.35).
narrative_ontology:measurement_basis(speech_absolutist_su_t1970, observed).
narrative_ontology:measurement(speech_absolutist_su_t1985, speech_protection_boundary__absolutist_reading, suppression_requirement, 1985, 0.37).
narrative_ontology:measurement_basis(speech_absolutist_su_t1985, observed).
narrative_ontology:measurement(speech_absolutist_su_t2000, speech_protection_boundary__absolutist_reading, suppression_requirement, 2000, 0.39).
narrative_ontology:measurement_basis(speech_absolutist_su_t2000, observed).
narrative_ontology:measurement(speech_absolutist_su_t2010, speech_protection_boundary__absolutist_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement_basis(speech_absolutist_su_t2010, observed).
narrative_ontology:measurement(speech_absolutist_su_t2018, speech_protection_boundary__absolutist_reading, suppression_requirement, 2018, 0.405).
narrative_ontology:measurement_basis(speech_absolutist_su_t2018, observed).
narrative_ontology:measurement(speech_absolutist_su_t2025, speech_protection_boundary__absolutist_reading, suppression_requirement, 2025, 0.41).
narrative_ontology:measurement_basis(speech_absolutist_su_t2025, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1970, tn=2025
narrative_ontology:measurement(speech_absolutist_grid_01, speech_protection_boundary__absolutist_reading, accessibility_collapse(class), 1970, 0.62).
narrative_ontology:measurement(speech_absolutist_grid_02, speech_protection_boundary__absolutist_reading, accessibility_collapse(class), 2025, 0.71).
narrative_ontology:measurement(speech_absolutist_grid_03, speech_protection_boundary__absolutist_reading, accessibility_collapse(individual), 1970, 0.68).
narrative_ontology:measurement(speech_absolutist_grid_04, speech_protection_boundary__absolutist_reading, accessibility_collapse(individual), 2025, 0.75).
narrative_ontology:measurement(speech_absolutist_grid_05, speech_protection_boundary__absolutist_reading, accessibility_collapse(organizational), 1970, 0.7).
narrative_ontology:measurement(speech_absolutist_grid_06, speech_protection_boundary__absolutist_reading, accessibility_collapse(organizational), 2025, 0.72).
narrative_ontology:measurement(speech_absolutist_grid_07, speech_protection_boundary__absolutist_reading, accessibility_collapse(structural), 1970, 0.75).
narrative_ontology:measurement(speech_absolutist_grid_08, speech_protection_boundary__absolutist_reading, accessibility_collapse(structural), 2025, 0.78).
narrative_ontology:measurement(speech_absolutist_grid_09, speech_protection_boundary__absolutist_reading, resistance(class), 1970, 0.82).
narrative_ontology:measurement(speech_absolutist_grid_10, speech_protection_boundary__absolutist_reading, resistance(class), 2025, 0.84).
narrative_ontology:measurement(speech_absolutist_grid_11, speech_protection_boundary__absolutist_reading, resistance(individual), 1970, 0.72).
narrative_ontology:measurement(speech_absolutist_grid_12, speech_protection_boundary__absolutist_reading, resistance(individual), 2025, 0.75).
narrative_ontology:measurement(speech_absolutist_grid_13, speech_protection_boundary__absolutist_reading, resistance(organizational), 1970, 0.68).
narrative_ontology:measurement(speech_absolutist_grid_14, speech_protection_boundary__absolutist_reading, resistance(organizational), 2025, 0.72).
narrative_ontology:measurement(speech_absolutist_grid_15, speech_protection_boundary__absolutist_reading, resistance(structural), 1970, 0.78).
narrative_ontology:measurement(speech_absolutist_grid_16, speech_protection_boundary__absolutist_reading, resistance(structural), 2025, 0.81).
narrative_ontology:measurement(speech_absolutist_grid_17, speech_protection_boundary__absolutist_reading, stakes_inflation(class), 1970, 0.55).
narrative_ontology:measurement(speech_absolutist_grid_18, speech_protection_boundary__absolutist_reading, stakes_inflation(class), 2025, 0.72).
narrative_ontology:measurement(speech_absolutist_grid_19, speech_protection_boundary__absolutist_reading, stakes_inflation(individual), 1970, 0.35).
narrative_ontology:measurement(speech_absolutist_grid_20, speech_protection_boundary__absolutist_reading, stakes_inflation(individual), 2025, 0.52).
narrative_ontology:measurement(speech_absolutist_grid_21, speech_protection_boundary__absolutist_reading, stakes_inflation(organizational), 1970, 0.28).
narrative_ontology:measurement(speech_absolutist_grid_22, speech_protection_boundary__absolutist_reading, stakes_inflation(organizational), 2025, 0.38).
narrative_ontology:measurement(speech_absolutist_grid_23, speech_protection_boundary__absolutist_reading, stakes_inflation(structural), 1970, 0.22).
narrative_ontology:measurement(speech_absolutist_grid_24, speech_protection_boundary__absolutist_reading, stakes_inflation(structural), 2025, 0.29).
narrative_ontology:measurement(speech_absolutist_grid_25, speech_protection_boundary__absolutist_reading, suppression(class), 1970, 0.48).
narrative_ontology:measurement(speech_absolutist_grid_26, speech_protection_boundary__absolutist_reading, suppression(class), 2025, 0.58).
narrative_ontology:measurement(speech_absolutist_grid_27, speech_protection_boundary__absolutist_reading, suppression(individual), 1970, 0.32).
narrative_ontology:measurement(speech_absolutist_grid_28, speech_protection_boundary__absolutist_reading, suppression(individual), 2025, 0.38).
narrative_ontology:measurement(speech_absolutist_grid_29, speech_protection_boundary__absolutist_reading, suppression(organizational), 1970, 0.25).
narrative_ontology:measurement(speech_absolutist_grid_30, speech_protection_boundary__absolutist_reading, suppression(organizational), 2025, 0.28).
narrative_ontology:measurement(speech_absolutist_grid_31, speech_protection_boundary__absolutist_reading, suppression(structural), 1970, 0.18).
narrative_ontology:measurement(speech_absolutist_grid_32, speech_protection_boundary__absolutist_reading, suppression(structural), 2025, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__absolutist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(speech_protection_boundary__absolutist_reading, 0.18).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, speech_protection_boundary__harm_limited_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, speech_protection_boundary__balancing_reading).

% DUAL FORMULATION NOTE:
% The speech_protection_boundary kernel decomposes into three readings: absolutist_reading (this file, maximal protection, Brandenburg standard), harm_limited_reading (speech conditional on absence of harm to equality and dignity), and balancing_reading (case-by-case weighing of First Amendment against competing values). Each reading has a different ε value because they measure the standing arrangement differently: absolutists measure Brandenburg's protection as near-costless coordination (low ε), harm advocates measure the same arrangement as high-extraction immunity (high ε). The readings do not disagree about what exists (the Brandenburg standard); they disagree about whether it should exist and what harm it enables. All three stories share the same referent (the Brandenburg boundary) and are reading-indexed: ε is an authored property per reading, not an observer-relative quantity. See DP-001 (ε-invariance principle) and kernel context section above.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
