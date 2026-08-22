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
 *   constraint_id: speech_protection_kernel__absolutist_reading
 *   human_readable: Near-Categorical Speech Protection (Absolutist Reading): Listener Harm Not Grounds for Restriction
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the speech-protection kernel: the
 *   absolutist_reading, under which speech protection operates
 *   near-categorically and listener harm is rejected as a ground for
 *   restriction, save a narrow fixed list of categorical exclusions
 *   (incitement, true threats, defamation, obscenity, fraud). The standing
 *   arrangement under contest — and therefore the epsilon referent, per the
 *   fixed kernel-reading rule — is that near-categorical protection order
 *   itself, assessed by this reading's own lights: a constructed
 *   constitutional command enforced by judicial review, under which the state
 *   is foreclosed from the entire class of harm-based speech restrictions,
 *   speakers hold the widest protection boundary in the reading set, and
 *   targets of harmful speech bear uncompensated costs that the reading
 *   classifies as the accepted price of the boundary rather than as a
 *   transfer to anyone. The sibling readings (harm_threshold, marketplace,
 *   dignity, democratic_participation) are separate constraint stories
 *   sharing the kernel text, linked through the network block; each authors
 *   its own epsilon, victim set, and boundary placement. This file authors
 *   only the absolutist reading. KEY AGENTS (by structural relationship): -
 *   constitutional_courts: agenda-setter/enforcer (institutional/constrained)
 *   — draws and polices the categorical boundary through judicial review -
 *   legislatures_and_regulators: bound party (institutional/trapped) —
 *   foreclosed from harm-based restriction; exit only by constitutional
 *   amendment - political_dissidents: primary beneficiary
 *   (powerless/constrained) — the paradigm protected class -
 *   unpopular_minority_speakers: beneficiary (powerless/constrained) —
 *   historically the first silenced under offense standards -
 *   offensive_speakers: beneficiary (powerless/constrained) — blasphemers,
 *   provocateurs, flag-burners; no narrower reading reaches them -
 *   targets_of_hate_speech: cost-bearer without remedy (powerless/trapped) —
 *   bears the accepted price; their claim is the one the reading rejects by
 *   construction - targets_of_harassing_offensive_speech: cost-bearer without
 *   remedy (powerless/trapped) — continuous exposure, no recourse short of
 *   the exclusions - general_public: dual seat (moderate/constrained) —
 *   beneficiary as listeners and potential speakers, diffuse cost-bearer as
 *   exposed audience - international_human_rights_bodies: excluded
 *   (institutional/trapped) — would restrict what this arrangement protects -
 *   comparative_law_scholars: analytical observer (moderate/analytical) —
 *   track the peer-democracy record testing the ratchet axiom
 *
 * KEY AGENTS:
 *   - constitutional_courts: agenda-setter/enforcer (institutional/constrained) — draws and polices the categorical boundary through judicial review; the arrangement exists only insofar as this seat maintains it
 *   - legislatures_and_regulators: bound party (institutional/trapped) — foreclosed from the entire class of harm-based speech restrictions; exit only via constitutional amendment
 *   - political_dissidents: primary beneficiary (powerless/constrained) — the paradigm protected class; first restricted under any harm standard, fully protected under this one
 *   - unpopular_minority_speakers: beneficiary (powerless/constrained) — collects the same wide boundary; historically the first silenced under offense standards
 *   - offensive_speakers: beneficiary (powerless/constrained) — blasphemers, provocateurs, flag-burners; no narrower reading extends protection to them
 *   - targets_of_hate_speech: cost-bearer without remedy (powerless/trapped) — bears the arrangement's accepted price; their claim to protection is the exact claim the reading rejects
 *   - targets_of_harassing_offensive_speech: cost-bearer without remedy (powerless/trapped) — continuous exposure in digital and physical spaces, no legal recourse short of the categorical exclusions
 *   - general_public: dual seat (moderate/constrained) — beneficiary as listeners and potential speakers, diffuse cost-bearer as the exposed audience
 *   - international_human_rights_bodies: excluded (institutional/trapped) — treaty bodies and peer jurisdictions that would restrict what this arrangement protects; structurally outside the constitutional conversation
 *   - comparative_law_scholars: analytical observer (moderate/analytical) — document the categorical boundary's outlier status and test the ratchet axiom against peer-democracy records
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__absolutist_reading, 0.15).
domain_priors:suppression_score(speech_protection_kernel__absolutist_reading, 0.25).
domain_priors:theater_ratio(speech_protection_kernel__absolutist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(speech_protection_kernel__absolutist_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__absolutist_reading, rope).
narrative_ontology:human_readable(speech_protection_kernel__absolutist_reading, "Near-Categorical Speech Protection (Absolutist Reading): Listener Harm Not Grounds for Restriction").
narrative_ontology:topic_domain(speech_protection_kernel__absolutist_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__absolutist_reading, '3807f2cb-a5bb-454e-b99a-cc95d37d2f18').
narrative_ontology:cs_kernel_codification('3807f2cb-a5bb-454e-b99a-cc95d37d2f18', fixed_text).
narrative_ontology:cs_authority_grounding('3807f2cb-a5bb-454e-b99a-cc95d37d2f18', lineage).
narrative_ontology:cs_interpretation_layer_present('3807f2cb-a5bb-454e-b99a-cc95d37d2f18').
narrative_ontology:cs_reading_relation('3807f2cb-a5bb-454e-b99a-cc95d37d2f18', speech_protection_kernel__harm_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('3807f2cb-a5bb-454e-b99a-cc95d37d2f18', speech_protection_kernel__dignity_reading, forecloses).
narrative_ontology:cs_reading_relation('3807f2cb-a5bb-454e-b99a-cc95d37d2f18', speech_protection_kernel__marketplace_reading, coexists_with).
narrative_ontology:cs_reading_relation('3807f2cb-a5bb-454e-b99a-cc95d37d2f18', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('3807f2cb-a5bb-454e-b99a-cc95d37d2f18', foundational, listener_harm_never_grounds_restriction).
narrative_ontology:cs_axiom_status(listener_harm_never_grounds_restriction, holdable).
narrative_ontology:cs_axiom_grounding('3807f2cb-a5bb-454e-b99a-cc95d37d2f18', listener_harm_never_grounds_restriction, deontological).
narrative_ontology:cs_axiom('3807f2cb-a5bb-454e-b99a-cc95d37d2f18', foundational, harm_exceptions_ratchet_into_censorship).
narrative_ontology:cs_axiom_status(harm_exceptions_ratchet_into_censorship, holdable).
narrative_ontology:cs_axiom_grounding('3807f2cb-a5bb-454e-b99a-cc95d37d2f18', harm_exceptions_ratchet_into_censorship, empirically_contingent).
narrative_ontology:cs_reference_frame('3807f2cb-a5bb-454e-b99a-cc95d37d2f18', no_law_means_no_law_baseline).
narrative_ontology:cs_drift_state('3807f2cb-a5bb-454e-b99a-cc95d37d2f18', contemporary_doctrinal_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3807f2cb-a5bb-454e-b99a-cc95d37d2f18', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__absolutist_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, political_dissidents).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, unpopular_minority_speakers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, offensive_speakers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__absolutist_reading, general_public).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, targets_of_hate_speech).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, targets_of_harassing_offensive_speech).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, legislatures_and_regulators).
narrative_ontology:constraint_victim(speech_protection_kernel__absolutist_reading, general_public).
narrative_ontology:constraint_vindicates(speech_protection_kernel__absolutist_reading, speaker_autonomy_principle).
narrative_ontology:constraint_vindicates(speech_protection_kernel__absolutist_reading, censorship_ratchet_thesis).
narrative_ontology:constraint_vindicates(speech_protection_kernel__absolutist_reading, unqualified_free_speech_command).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Review every speech restriction against the categorical command, strike down harm-based restrictions, and maintain the narrow list of categorical exclusions. The boundary exists only insofar as this seat keeps drawing and policing it. Revising doctrine from the bench is possible at the margins but binds the seat to precedent and institutional legitimacy.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, constitutional_courts, agenda_setter,
    institutional, generational, constrained, national).

% Enact speech regulation in response to constituent demand — hate speech, harassment, misinformation, offense. Every harm-based restriction is struck down under the categorical command, foreclosing the entire class of listener-harm grounds. The only exit is constitutional amendment, which the command's supermajority requirements place effectively out of reach.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, legislatures_and_regulators, payer,
    institutional, biographical, trapped, national).

% Speak against the state and its majorities. They are the paradigm class the categorical rule exists to protect: under any harm standard their speech is restricted first, so the categorical refusal is worth most to them. Leaving the jurisdiction would surrender the protection, and their speech is precisely what makes them targets.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, political_dissidents, beneficiary,
    powerless, biographical, constrained, national).

% Members of minority groups speaking about their own lives and claims; historically the first restricted under harm and offense standards invoked by majorities. They collect the same wide protection boundary, and their exit options are constrained by the same logic as dissidents'.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, unpopular_minority_speakers, beneficiary,
    powerless, biographical, constrained, national).

% Speakers whose expression is profane, blasphemous, or deliberately provocative toward ideas, symbols, and institutions. No narrower protection boundary in the reading set extends to them. Their social standing is low and their legal protection under this arrangement is total.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, offensive_speakers, beneficiary,
    powerless, biographical, constrained, national).

% Members of groups targeted by demeaning group-directed expression. The speech is protected under the categorical command; they have no legal remedy, cannot exit the community where the speech circulates, and bear the harm as the arrangement's accepted price. Their claim to protection is the exact claim the reading rejects by construction.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, targets_of_hate_speech, payer,
    powerless, biographical, trapped, national).

% Individuals subjected to harassing, demeaning, or distressing expression that falls short of the categorical exclusions — not a true threat, not defamation. No remedy is available; exposure is continuous across digital and physical spaces; exit would mean withdrawing from public discourse altogether.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, targets_of_harassing_offensive_speech, payer,
    powerless, biographical, trapped, national).

% Hear unfiltered speech and hold the listener's side of the categorical protection — the right to receive expression. Every member is a potential speaker covered by the boundary. They also absorb the diffuse cost of exposure to harmful, false, and offensive expression that no restriction under this arrangement touches.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, general_public, beneficiary,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__absolutist_reading, general_public, payer).

% Treaty bodies and peer jurisdictions that classify group-directed hateful speech as restrictable. Their norms have no purchase inside the constitutional order, which binds only its own command. They would restrict what this arrangement protects; their exclusion from the constitutional conversation is structural.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, international_human_rights_bodies, excluded,
    institutional, generational, trapped, global).

% Document that the categorical boundary is an outlier among peer democracies and track whether harm-based regimes elsewhere remain bounded or generalize over decades — the empirical record on which this reading's ratchet axiom rests.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__absolutist_reading, comparative_law_scholars, observer,
    moderate, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__absolutist_reading, diffuse).
narrative_ontology:fixing_cost_class(speech_protection_kernel__absolutist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the censorship collective-action problem: once any faction can invoke 'harm' to restrict speech that offends it, every faction acquires the same weapon, and restriction expands ratchet-like until only official and majority-safe speech remains. The near-categorical rule removes the entire class of harm-based restrictions, stabilizing an equilibrium in which all speakers — including the state's critics — hold the same protection boundary.
% TRANSFER_FUNCTION: Moves nothing between parties. The arrangement forbids a class of state action rather than transferring money, work, attention, or status. Its benefits accrue as liberty to speakers diffusely; its costs are uncompensated externalities borne by targets of harmful speech and collected by no seat.
% ABSENT_VOICES: Targets of hate speech and harassment would object that their harm is dismissed as non-grounds by construction; they sit inside the polity but outside the constraint's terms — the rule is built so their testimony cannot ground restriction. International human rights bodies and peer democracies that restrict hateful speech are excluded from the constitutional conversation entirely, with no jurisdictional purchase inside the order.
% DISAPPEARANCE_RATIONALE: If near-categorical protection vanished overnight, legislatures would enact harm-based restrictions within a session — hate speech, offense, harassment, misinformation — each defined by whichever faction holds power. Speakers would chill toward the safe center, and dissident and minority speech would bear the first restrictions. The speech environment would reorganize around state-drawn harm lines.
% FOUNDING_PROBLEM: Governments and majorities historically weaponize 'harm' to silence critics: sedition, blasphemy, obscenity, and subversion prosecutions were the norm. The categorical rule was built to remove the harm justification entirely, so the state could never claim a harm warrant for censorship again.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship on sedition, blasphemy, and obscenity prosecutions attests the founding problem from outside the speaker beneficiary set, and the recurring cross-faction pattern — each governing faction proposing restrictions on its opponents' speech once in power, documented in legislative records across jurisdictions — corroborates that the harm-weaponization problem remains live. The constitutional courts, an agenda-setter seat outside the beneficiary set, restate the problem in every restriction decision. Targets' advocacy groups dispute only the reading's remedy, not the historical problem.
narrative_ontology:disappearance_verdict(speech_protection_kernel__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__absolutist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_kernel__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__absolutist_reading, 0.15, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored low (0.15, the series end-state) because the reading's own accounting finds no collector: the arrangement forbids a class of state action rather than transferring value, the state's foreclosed power is received by no seat, and the harm to targets is an uncompensated externality the reading explicitly declines to count as extraction — that declination is the reading's defining move, and it is marked rather than settled by the silencing_externality_status omega. Suppression (0.25) reflects real coercive enforcement — courts strike down enacted laws — aimed at state action the reading counts as the constraint's function rather than a cost. Theater ratio is low (0.10) because the protection is operative: restrictions are actually struck and speakers actually rely on the boundary; the small theatrical component is the gap between categorical rhetoric ('no law means no law') and the carve-out practice the seat actually administers, which is why theater rises slowly across the series. Accessibility collapse (0.75) is assessed from the bound party's seat: once the categorical command is understood, the state's harm-based regulatory alternatives collapse almost completely, with the narrow exclusions and content-neutral regulation surviving as residual option space. Resistance (0.65) is high and constant — legislatures retest the boundary every session, targets litigate, treaty bodies and peer democracies press from outside — which is precisely why the reading holds the rule must be categorical rather than balanced. The claimed type (rope) and the metrics are authored independently: the reading claims a genuine coordination solution to the censorship collective-action problem with net benefit to participants; the engine computes per-seat types from the structural data, and divergence at the payer seats is the datum, not an error. All three measurement series run on one shared grid (0, 16, 32, 48, 64, 80) with every metric authored at every point; the suppression series is authored because the story specifically tracks the enforcement apparatus maturing from weak early-era review to hardened, sustained enforcement, plateauing as the machinery stabilizes.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the constitutional_courts seat the arrangement is a workable doctrinal line the seat itself draws and polices. From political_dissidents and unpopular_minority_speakers it is a lifeline: under any harm standard their speech is restricted first, so the categorical refusal is worth more to them than to any other seat. From legislatures_and_regulators it is the removal of an entire regulatory class they are democratically pressed to use — but they are bound by design, which is subjection to a command rather than extraction-victimhood. From targets_of_hate_speech and targets_of_harassing_offensive_speech the same structure is experienced as abandonment: their harm is defined as non-grounds by the very rule that protects everyone else, and their trapped position (no remedy, no exit from the speech environment) places them at the target end of the directionality spectrum. The engine computes this divergence from the role, power, and exit data; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (political_dissidents, unpopular_minority_speakers, offensive_speakers, general_public) derive low d for those seats — the arrangement subsidizes their expression. Victim declarations (targets_of_hate_speech, targets_of_harassing_offensive_speech) derive high d, pushed toward the full-target end by trapped exit: no remedy and no exit from the environment where the speech circulates. general_public carries a dual role and lands near symmetric: genuine listening benefit, diffuse exposure cost. legislatures_and_regulators are payers by role and trapped by exit, deriving high d — structurally accurate, since the constraint is aimed at them; the reading's dispute is not with that d but with whether being bound counts as being extracted from. constitutional_courts are declared neither beneficiary nor victim; they administer the boundary and expand their own authority in doing so, sitting near symmetric with a slight beneficiary lean. No directionality overrides are used: the derivation from role declarations plus exit options captures the structure, and the available overrides are keyed by power atom, which could not separate the two institutional seats whose divergence is role-based rather than power-based.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — state and majority weaponization of 'harm' to silence critics (sedition, blasphemy, obscenity prosecutions) — is authored live: every political era generates new harm-based restriction demands, and the cross-faction pattern in legislative records corroborates the problem from outside the speaker beneficiary set. With the mandate live, no mandatrophy arises, and the arrangement's persistence tracks an operative function rather than inertia. The classification guards against two mislabels in opposite directions: against snare, by the receipt surface — the arrangement names cost-bearers but no seat collects, and extraction without a collector is externality, not extraction (the silencing_externality_status omega marks the contingency under which that accounting could fail); against mountain, by emerges_naturally: false — the boundary is a constructed constitutional commitment, not a natural law, however bedrock-like its rhetoric. Piton is excluded by low theater and an operative enforcement function. Should the ratchet axiom be empirically refuted, the mandate becomes contested rather than dead — the arrangement would then persist partly on tradition, and reclassification pressure toward tangled_rope would follow.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the speech_protection_kernel (the absolutist_reading). What would each sibling reading change structurally if instantiated instead of this one?',
    'Author each sibling reading as its own constraint story against the same referent and compare epsilon, victim sets, boundary placement, and per-seat classifications across the family.',
    'The harm_threshold and dignity readings would convert this arrangement''s uncompensated cost-bearers into declared victims with actionable claims and move the boundary inward, raising epsilon at the same referent; the marketplace and democratic_participation readings would keep a wide boundary but re-ground it, leaving the victim set roughly intact while changing the vindicated propositions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: reading indexicality of the speech protection kernel — what each sibling reading would change.').

omega_variable(
    disagreement_location,
    'Where is the family disagreement located — in the empirical ratchet claim (harm exceptions generalize into censorship), in the deontological priority of speaker autonomy, or in the moral status of listener harm itself?',
    'Conceptual separation of the reading''s two axioms: test the empirically_contingent ratchet axiom against comparative data independently of the deontological autonomy axiom, and ask which strand each sibling''s objection actually targets.',
    'If the disagreement is located in the empirical strand, comparative evidence could move the boundary and weaken this reading''s foreclosure of the conditional readings; if located in the deontological strand, no empirical resolution is available and the readings remain permanently co-live as rival frameworks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disagreement_location, conceptual, 'Location of the inter-reading disagreement: empirical ratchet claim versus deontological autonomy core.').

omega_variable(
    silencing_externality_status,
    'Does protected harmful speech functionally silence its targets'' own speech (chilling of target-group expression), such that the arrangement transfers expressive liberty from targets to their harassers and some seat does capture gains?',
    'Empirical study of target-group speech participation before and after exposure to protected hostile speech, and of community-level participation shifts where hostile speech is moderated.',
    'If silencing is substantial, the no-collector accounting fails: the arrangement''s beneficiaries would include agents whose gain is others'' suppressed speech, effective extraction rises, and per-seat classification pressure shifts toward tangled_rope or snare at the margin; if negligible, the externality reading holds and the low-epsilon authoring stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(silencing_externality_status, empirical, 'Whether the uncompensated harm is pure externality or a second-order transfer with a collector — the strongest internal challenge to this reading''s accounting.').

omega_variable(
    ratchet_axiom_empirical_status,
    'Does permitting harm-based speech restriction actually ratchet into broad censorship (this reading''s empirically contingent axiom), or do peer democracies'' harm-based regimes remain bounded over time?',
    'Longitudinal comparative constitutional data: the scope of hate-speech and harm-based restrictions in peer jurisdictions across decades — whether restriction categories have generalized or remained bounded.',
    'If the ratchet is refuted, the categorical rule loses its consequentialist grounding and rests on the deontological core alone; this reading''s foreclosure of the conditional readings weakens, fixing_cost would reassess from prohibitive, and the arrangement''s persistence would carry tradition-weight that pressures reclassification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ratchet_axiom_empirical_status, empirical, 'Empirical status of the censorship-ratchet premise underlying categorical protection.').

omega_variable(
    categorical_exclusion_boundary,
    'Where exactly does the narrow set of categorical exclusions sit, and is it stable — could new carve-outs (for misinformation, group harassment, or similar) widen the exclusion set and move the effective boundary inward?',
    'Doctrinal tracking of the exclusion set across the interval, and of proposed carve-outs in pending legislation and litigation.',
    'Each widening of the exclusions shrinks the uncompensated-harm class (lowering the externality cost) while narrowing speaker protection (raising the cost this reading counts against itself); sustained widening would date a practice_drift transition and erode the near-categorical claim this story authors.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(categorical_exclusion_boundary, empirical, 'Stability of the categorical exclusion set that fixes the protection boundary.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__absolutist_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spee_tr_t0, speech_protection_kernel__absolutist_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(spee_tr_t16, speech_protection_kernel__absolutist_reading, theater_ratio, 16, 0.1).
narrative_ontology:measurement(spee_tr_t32, speech_protection_kernel__absolutist_reading, theater_ratio, 32, 0.12).
narrative_ontology:measurement(spee_tr_t48, speech_protection_kernel__absolutist_reading, theater_ratio, 48, 0.14).
narrative_ontology:measurement(spee_tr_t64, speech_protection_kernel__absolutist_reading, theater_ratio, 64, 0.16).
narrative_ontology:measurement(spee_tr_t80, speech_protection_kernel__absolutist_reading, theater_ratio, 80, 0.18).

% Extraction over time
narrative_ontology:measurement(spee_be_t0, speech_protection_kernel__absolutist_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(spee_be_t16, speech_protection_kernel__absolutist_reading, base_extractiveness, 16, 0.11).
narrative_ontology:measurement(spee_be_t32, speech_protection_kernel__absolutist_reading, base_extractiveness, 32, 0.12).
narrative_ontology:measurement(spee_be_t48, speech_protection_kernel__absolutist_reading, base_extractiveness, 48, 0.13).
narrative_ontology:measurement(spee_be_t64, speech_protection_kernel__absolutist_reading, base_extractiveness, 64, 0.14).
narrative_ontology:measurement(spee_be_t80, speech_protection_kernel__absolutist_reading, base_extractiveness, 80, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(spee_su_t0, speech_protection_kernel__absolutist_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(spee_su_t16, speech_protection_kernel__absolutist_reading, suppression_requirement, 16, 0.2).
narrative_ontology:measurement(spee_su_t32, speech_protection_kernel__absolutist_reading, suppression_requirement, 32, 0.28).
narrative_ontology:measurement(spee_su_t48, speech_protection_kernel__absolutist_reading, suppression_requirement, 48, 0.33).
narrative_ontology:measurement(spee_su_t64, speech_protection_kernel__absolutist_reading, suppression_requirement, 64, 0.35).
narrative_ontology:measurement(spee_su_t80, speech_protection_kernel__absolutist_reading, suppression_requirement, 80, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__absolutist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__absolutist_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'speech protection' covers structurally distinct claims and decomposes into a five-story constraint family sharing one kernel text: the absolutist reading (this story — widest boundary, autonomy-grounded, listener harm rejected as grounds), the harm-threshold and dignity readings (conditional boundaries that convert this story's cost-bearers into declared victims), and the marketplace and democratic_participation readings (wide boundaries re-grounded in truth-discovery and self-governance). Each member authors its own epsilon, victim set, and type against the same referent. The family's upstream/downstream structure runs from this reading outward: the conditional readings are downstream responses to this reading's uncompensated-harm costs, and this reading's ratchet axiom is the empirical premise their proponents attack. All five files link one another through affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
