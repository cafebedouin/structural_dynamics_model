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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: speech_protection_kernel__marketplace_reading
 *   human_readable: Marketplace Reading of Speech Protection — Truth-Discovery Warrant and Counter-Speech Doctrine
 *   domain: constitutional_law/political_philosophy/communication_rights
 *
 * SUMMARY:
 *   The marketplace reading instantiates speech protection as an epistemic
 *   instrument: expression is protected because unrestricted contestation is
 *   how communities discover truth, and content-based restrictions are
 *   condemned as prejudging outcomes the process exists to determine.
 *   Falsehood and hostile speech are answered with more speech rather than
 *   removal. The arrangement has a genuine coordination function — it
 *   prevents every governing majority from closing the epistemic commons
 *   around its own certainties — and a genuine transfer: the costs of
 *   unfiltered expression concentrate on identifiable targets and publics
 *   while the benefits spread across speakers, publishers, and the commons at
 *   large, with judicial machinery actively striking down restriction
 *   attempts. FAMILY NOTE: the colloquial label 'free speech' decomposes into
 *   five structurally distinct readings of one kernel; this file authors ONLY
 *   the marketplace reading with its own stable epsilon, beneficiary/victim
 *   sets, and classification. Sibling readings are separate constraints,
 *   linked via network.affects_constraints; their victim sets and remedy
 *   legitimacies differ (see the sibling_reading_structural_delta omega). The
 *   claimed type and the metrics are independent authored facts: the
 *   tangled_rope claim comes from the structural read (real coordination
 *   function + asymmetric enforced transfer), the metric values from the
 *   arrangement's observed operation.
 *
 * KEY AGENTS:
 *   - judiciary_enforcing_content_neutrality: agenda-setting enforcer (institutional/constrained) — administers the doctrine, strikes down restriction attempts, collects no rents
 *   - dissenting_speakers_and_minority_viewpoints: primary beneficiary (moderate/constrained) — the classic Millian protected class
 *   - platform_publishers_and_mass_media: institutional beneficiary (powerful/arbitrage) — converts protection into distribution revenue; receipt seat for the arrangement's gains
 *   - academic_and_scientific_communities: beneficiary (organized/constrained) — depends on open challenge for revision
 *   - harassment_and_defamation_targets: primary payer (moderate/trapped) — bears concentrated personal costs, denied the restriction remedy
 *   - marginalized_groups_facing_hostile_speech: payer (organized/identity_locked) — exposure constituted by group membership
 *   - misinformation_exposed_public: payer (powerless/trapped) — absorbs diffuse epistemic costs with no lever
 *   - counter_speech_institutions: derivative beneficiary (organized/mobile) — their function is created by the doctrine's remedy choice
 *   - harm_and_dignity_restriction_advocates: excluded voice (organized/constrained) — their remedy is predefined as illegitimate inside the frame
 *   - free_expression_theorists: analytical observer (analytical/analytical) — sees the full cross-reading structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__marketplace_reading, 0.58).
domain_priors:suppression_score(speech_protection_kernel__marketplace_reading, 0.62).
domain_priors:theater_ratio(speech_protection_kernel__marketplace_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(speech_protection_kernel__marketplace_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__marketplace_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__marketplace_reading, "Marketplace Reading of Speech Protection — Truth-Discovery Warrant and Counter-Speech Doctrine").
narrative_ontology:topic_domain(speech_protection_kernel__marketplace_reading, "constitutional_law/political_philosophy/communication_rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__marketplace_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__marketplace_reading, '291c881b-ad10-4e67-85d3-22436ea7d0de').
narrative_ontology:cs_kernel_codification('291c881b-ad10-4e67-85d3-22436ea7d0de', fixed_text).
narrative_ontology:cs_authority_grounding('291c881b-ad10-4e67-85d3-22436ea7d0de', lineage).
narrative_ontology:cs_interpretation_layer_present('291c881b-ad10-4e67-85d3-22436ea7d0de').
narrative_ontology:cs_reading_relation('291c881b-ad10-4e67-85d3-22436ea7d0de', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('291c881b-ad10-4e67-85d3-22436ea7d0de', speech_protection_kernel__harm_threshold_reading, influences).
narrative_ontology:cs_reading_relation('291c881b-ad10-4e67-85d3-22436ea7d0de', speech_protection_kernel__dignity_reading, influences).
narrative_ontology:cs_reading_relation('291c881b-ad10-4e67-85d3-22436ea7d0de', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('291c881b-ad10-4e67-85d3-22436ea7d0de', foundational, truth_discovery_justifies_protection).
narrative_ontology:cs_axiom_status(truth_discovery_justifies_protection, holdable).
narrative_ontology:cs_axiom_grounding('291c881b-ad10-4e67-85d3-22436ea7d0de', truth_discovery_justifies_protection, instrumental).
narrative_ontology:cs_axiom('291c881b-ad10-4e67-85d3-22436ea7d0de', foundational, content_restrictions_distort_discovery).
narrative_ontology:cs_axiom_status(content_restrictions_distort_discovery, holdable).
narrative_ontology:cs_axiom_grounding('291c881b-ad10-4e67-85d3-22436ea7d0de', content_restrictions_distort_discovery, empirically_contingent).
narrative_ontology:cs_reference_frame('291c881b-ad10-4e67-85d3-22436ea7d0de', millian_open_contestation_forum).
narrative_ontology:cs_drift_state('291c881b-ad10-4e67-85d3-22436ea7d0de', contemporary_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('291c881b-ad10-4e67-85d3-22436ea7d0de', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__marketplace_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, dissenting_speakers_and_minority_viewpoints).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, platform_publishers_and_mass_media).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, academic_and_scientific_communities).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__marketplace_reading, counter_speech_institutions).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, harassment_and_defamation_targets).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, misinformation_exposed_public).
narrative_ontology:constraint_victim(speech_protection_kernel__marketplace_reading, marginalized_groups_facing_hostile_speech).
narrative_ontology:constraint_vindicates(speech_protection_kernel__marketplace_reading, millian_epistemic_argument).
narrative_ontology:constraint_vindicates(speech_protection_kernel__marketplace_reading, brandeisian_counter_speech_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Courts interpret and apply the protection doctrine, strike down content-based restriction attempts, and articulate the counter-speech rationale in successive rulings. They collect no direct payment from the arrangement and bear no personal exposure to the speech it shields; their cost is doctrinal effort, and shifting course requires overturning settled precedent line by line, so the administration of the doctrine binds panels far beyond any single judge's tenure.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, judiciary_enforcing_content_neutrality, agenda_setter,
    institutional, generational, constrained, national).

% Hold views that current majorities, officials, or institutional gatekeepers would suppress if given the tool. The protection regime is what keeps their expression lawful without pre-clearance or licensing. They can withdraw from public debate, but they cannot carry the protection with them into jurisdictions or communities that do not share the doctrine.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, dissenting_speakers_and_minority_viewpoints, beneficiary,
    moderate, biographical, constrained, national).

% Operate channels whose economics depend on unrestricted publication and maximal engagement. The protection regime lets them distribute all comers' expression while the costs of responding to harmful content fall on targets, volunteer moderators, and audiences. They can relocate incorporation, rewrite terms of service, or shift operations across jurisdictions faster than doctrine can follow, and they monetize the attention that hostile and false expression generates.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, platform_publishers_and_mass_media, beneficiary,
    powerful, biographical, arbitrage, global).

% Depend on open challenge to revise received results; the regime guarantees room for unwelcome findings and heterodox methods. Leaving would mean moving to systems where publication requires approvals they do not control, so they stay and defend the arrangement through professional norms and editorial practice.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, academic_and_scientific_communities, beneficiary,
    organized, generational, constrained, global).

% Bear concentrated, personal costs of protected hostile expression: reputational damage, threats, sustained harassment campaigns, fabricated accusations. The remedy they most directly seek — removal or restriction of the specific content — is precisely what the doctrine defines as illegitimate. Leaving the platform or the public sphere is costly, and the speech follows them; counter-speech on their behalf arrives late, reaches fewer people than the original claim, and must be repeated indefinitely.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, harassment_and_defamation_targets, payer,
    moderate, immediate, trapped, national).

% Membership groups repeatedly targeted by demeaning or threatening expression. Advocacy organizations press for restriction and receive counter-speech resources instead. Individual members cannot exit the identity that attracts the targeting — the exposure is constituted by who they are, not by a channel they chose. The group's standing in public discourse is itself part of what the open contestation continually renegotiates.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, marginalized_groups_facing_hostile_speech, payer,
    organized, generational, identity_locked, national).

% Absorb the epistemic costs of protected falsehood: health misinformation, fabricated claims during elections, coordinated deception campaigns. They have no lever to restrict the supply, limited individual capacity to counter it, and no practical way to opt out of the shared information environment on which civic life runs.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, misinformation_exposed_public, payer,
    powerless, immediate, trapped, national).

% Fact-checking networks, annotation systems, and rebuttal outlets exist because the doctrine routes responses to bad speech through added speech rather than removal. They receive funding, platform integration, and professional standing from the demand the doctrine creates, and they can pivot to adjacent verification markets if the doctrinal settlement shifts.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, counter_speech_institutions, beneficiary,
    organized, biographical, mobile, global).

% Scholars, victims' organizations, and dignity-focused jurists who argue that demonstrable harm or structural subordination should license restriction. Inside the dominant doctrinal frame their proposed remedies are treated as category errors rather than live options; they publish, litigate at the margins, and wait for doctrinal openings, with their strongest arguments never reaching the decision procedure that governs the arrangement.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, harm_and_dignity_restriction_advocates, excluded,
    organized, generational, constrained, national).

% Map the competing justifications for speech protection, track where the counter-speech mechanism succeeds or fails empirically, and compare jurisdictions with different settlements. Positioned outside the enforcement loop, they see the full structure of who pays and who benefits across the whole family of readings.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__marketplace_reading, free_expression_theorists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__marketplace_reading, platform_publishers_and_mass_media).
narrative_ontology:fixing_cost_class(speech_protection_kernel__marketplace_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps the epistemic commons open by denying any authority the power to adjudicate truth in advance; solves the recurring problem that governing majorities mistake their own certainties for final answers and suppress challenges before the challenges can prove themselves.
% TRANSFER_FUNCTION: Moves expressive liberty to all speakers — including purveyors of falsehood and hostile expression — and moves the burden of answering harmful speech from the state's censor onto targets, volunteers, and counter-speech institutions; moves reputational risk and attention costs onto listeners and the targeted.
% ABSENT_VOICES: Harassment and defamation targets, communities bearing disinformation costs, and dignity-focused jurists would object that the arrangement purchases collective epistemic benefit with their unbudgeted personal costs. They stand outside the doctrinal conversation, where their proposed remedy — restriction — is classified in advance as a distortion of the process rather than entertained as a claim.
% DISAPPEARANCE_RATIONALE: Overnight repeal would rearrange the expressive order immediately: legislatures and platforms would begin restricting by content within the news cycle, each community sealing its commons around local certainties; dissenting speakers would lose the only protection that does not depend on majority goodwill; platform distribution policies would reorganize around liability avoidance; and the epistemic commons would fragment along jurisdictional and communal lines.
% FOUNDING_PROBLEM: The recurring historical pattern in which authorities — state, clerical, or majoritarian — suppressed true challenges to orthodoxy because they were certain the challenge was false, destroying the discoveries the challenge would have produced.
% FOUNDING_PROBLEM_CORROBORATION: Historians of science documenting censorship-suppressed-then-vindicated cases, comparative indices of expression across restrictive jurisdictions, and testimony of dissidents publishing under systems lacking such protection — sources that collect no rent from this arrangement's operation. Corroboration from inside the beneficiary set (publishers, platforms, professional free-speech bodies) is acknowledged as self-interested and weighted accordingly.
narrative_ontology:disappearance_verdict(speech_protection_kernel__marketplace_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__marketplace_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__marketplace_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_kernel__marketplace_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__marketplace_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction 0.58: real, recurring costs imposed on identifiable parties who did not consent and cannot refuse — but partially reciprocal, since most speakers are also listeners and today's target is tomorrow's protected dissenter, which holds it below snare-grade. Suppression 0.62 (raw structural property, NOT scaled by power or scope — only extractiveness is scaled downstream): the arrangement persists through active judicial enforcement that denies restriction-seeking parties their preferred remedy; alternatives are not erased — sibling readings persist in scholarship and occasional doctrine — but are systematically ruled out inside the enforcing institutions. Theater 0.32: the counter-speech doctrine is increasingly invoked in settings where counter-speech capacity is absent, so the ratio of rhetorical invocation to operative remedy grows as attention concentrates on few channels. Accessibility_collapse 0.55: once the doctrine is understood, restriction-based alternatives collapse substantially within the jurisdiction, though incompletely — harm and dignity readings retain footholds at the margins. Resistance 0.65: continuous organized pressure from targets' advocates, dignity scholars, and disinformation-response movements; the partial organization of the payer seats (advocacy coalitions) is exactly what keeps resistance this high despite individually powerless members. The measurement series run on one shared time grid — every tracked metric is authored at every examined time point — with end-state values matching the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat and the payer seats should compute very differently. From the bench, the arrangement reads as the maintenance of a hard-won epistemic order: the same ruling that protects the commons reads, from a harassment target's seat, as abandonment to her harassers, and from a disinformation-exposed public's seat as a refusal to fence a known hazard. Beneficiary seats split too: the dissident experiences a guarantee purchased at no one's expense; the platform experiences a subsidy it converts to revenue while externalizing harm costs. Identity-lock dynamics concentrate on the marginalized-group seat: the fusion here is ascribed and relational — membership in the targeted group constitutes the exposure, so exit would require ceasing to be a member, which is not an available act. If targeting ceased to track group membership, that seat's exit would loosen to constrained and its effective extraction would drop sharply. The engine computes these per-seat divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low d: dissenting speakers, academic communities, and counter-speech institutions sit near the subsidized end; platform publishers sit nearest zero, converting the protection directly into distribution revenue while harm-response costs land elsewhere. Victim declarations map to high d: harassment and defamation targets and the misinformation-exposed public sit near the full-target end, amplified by trapped exits — the remedy they seek is the thing the arrangement withholds, so no mobility relieves their position; marginalized groups join them with identity-lock binding exit further. The judiciary sits near symmetric: it administers without collecting, paying in doctrinal effort what it receives in institutional purpose. Excluded advocates register as outside the derivation entirely — their exclusion is the enforcement object's shadow, not a measured seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both failure modes. Reading the arrangement as pure coordination would erase the concentrated, non-consenting cost-bearers: the harassment target is not paying coordination overhead, she is paying the price of someone else's epistemology. Reading it as pure extraction would erase the genuine function: the historical record of censorship suppressing true challenges is real, and the beneficiary set includes the politically powerless dissenter alongside the platform. The tangled_rope claim holds both facts together — coordination function live (founding problem corroborated by censorship history from outside the beneficiary set), transfer asymmetric and actively enforced. Mandatrophy is not resolved: the founding problem remains live wherever authorities claim prior knowledge of truth, so the arrangement has not outlived its mandate. The watch item is the theater trajectory: rising theater_ratio traces the counter-speech doctrine decoupling from counter-speech capacity. If invocation continues outrunning remedy, the arrangement drifts toward maintaining the story of the mechanism rather than the mechanism — the classic degraded-inertia signature — and the payer seats' costs would persist while the coordinating function became performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_structural_delta,
    'This constraint is one reading of the speech_protection_kernel (the marketplace_reading); which structural elements change under the sibling readings, and where exactly is the disagreement located?',
    'Cross-reading comparison of the four sibling stories: diff the victim sets and remedy legitimacies (absolutist confirms listener-harm costs as permanent epistemic overhead; harm_threshold grants restriction remedies upon demonstrable harm; dignity grants remedies where expression functions as subordination; democratic_participation narrows the protection warrant to political expression) and recompute epsilon per reading over the shared referent.',
    'Under harm_threshold or dignity readings, harassment and defamation targets gain legitimate restriction remedies and their effective extraction falls sharply; under the absolutist reading their costs are ratified as permanent; under democratic_participation, non-political hostile expression loses its protection warrant entirely. The disagreement is located in the justification basis (collective epistemic benefit vs autonomy vs harm-avoidance vs dignity vs self-governance), which determines whose costs count.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer structure: this story is one of five readings; the readings partition on which costs are legitimate to impose.').

omega_variable(
    counter_speech_efficacy_asymmetry,
    'Does the counter-speech remedy actually neutralize harmful falsehood for speakers and targets who lack platform-scale reach, or does it function mainly for those with existing audiences?',
    'Audit rebuttal reach versus original-claim reach across platform datasets; measure correction latency and audience overlap for claims targeting low-profile individuals and groups versus high-profile figures.',
    'If counter-speech systematically fails for the low-reach, the doctrine''s functional share shrinks, the authored theater_ratio is understated, and the arrangement drifts toward maintaining a coordination story whose operative mechanism serves only the well-amplified — pushing the payer seats'' effective extraction upward without any change in the doctrine''s stated terms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_speech_efficacy_asymmetry, empirical, 'Whether the ''more speech'' remedy works symmetrically across audience sizes.').

omega_variable(
    algorithmic_amplification_inversion,
    'Did algorithmic curation break the marketplace premise that falsehood and correction compete on roughly equal terms?',
    'Comparative virality studies of false versus corrective content under ranked feeds versus chronological distribution; longitudinal tracking of correction-to-claim reach ratios.',
    'If amplification reliably favors the false, the reading''s core empirical premise is inverted at scale, and the extraction profile shifts from diffuse epistemic cost toward concentrated manipulation of the misinformation-exposed public — strengthening the case that the arrangement now transfers more than it coordinates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_amplification_inversion, empirical, 'Platform-era validity of the equal-contestation premise underlying the truth-discovery warrant.').

omega_variable(
    private_curation_boundary,
    'Does the reading''s anti-distortion warrant extend to private platform curation, or does privately curated distribution constitute the very content-based distortion the reading condemns?',
    'Conceptual analysis distinguishing government restriction (the reading''s traditional enforcement target) from private gatekeeping shaping the same epistemic commons; test whether the truth-discovery justification survives when the curator is commercial and optimization-driven.',
    'If private curation counts as distortion, the reading''s own logic indicts dominant platform practices and its beneficiary structure partially inverts (platforms become targets of the doctrine they profit under); if not, the doctrine protects expression while tolerating a larger distortion of the commons than any government imposed — a scope boundary the sibling readings resolve differently.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(private_curation_boundary, conceptual, 'Scope boundary of the reading''s anti-distortion warrant across public and private curators.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__marketplace_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spk_mktplace_tr_t0, speech_protection_kernel__marketplace_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(spk_mktplace_tr_t0, observed).
narrative_ontology:measurement(spk_mktplace_tr_t5, speech_protection_kernel__marketplace_reading, theater_ratio, 5, 0.17).
narrative_ontology:measurement_basis(spk_mktplace_tr_t5, observed).
narrative_ontology:measurement(spk_mktplace_tr_t10, speech_protection_kernel__marketplace_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement_basis(spk_mktplace_tr_t10, observed).
narrative_ontology:measurement(spk_mktplace_tr_t15, speech_protection_kernel__marketplace_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement_basis(spk_mktplace_tr_t15, observed).
narrative_ontology:measurement(spk_mktplace_tr_t20, speech_protection_kernel__marketplace_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement_basis(spk_mktplace_tr_t20, observed).
narrative_ontology:measurement(spk_mktplace_tr_t25, speech_protection_kernel__marketplace_reading, theater_ratio, 25, 0.29).
narrative_ontology:measurement_basis(spk_mktplace_tr_t25, observed).
narrative_ontology:measurement(spk_mktplace_tr_t30, speech_protection_kernel__marketplace_reading, theater_ratio, 30, 0.32).
narrative_ontology:measurement_basis(spk_mktplace_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(spk_mktplace_be_t0, speech_protection_kernel__marketplace_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(spk_mktplace_be_t0, observed).
narrative_ontology:measurement(spk_mktplace_be_t5, speech_protection_kernel__marketplace_reading, base_extractiveness, 5, 0.47).
narrative_ontology:measurement_basis(spk_mktplace_be_t5, observed).
narrative_ontology:measurement(spk_mktplace_be_t10, speech_protection_kernel__marketplace_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement_basis(spk_mktplace_be_t10, observed).
narrative_ontology:measurement(spk_mktplace_be_t15, speech_protection_kernel__marketplace_reading, base_extractiveness, 15, 0.53).
narrative_ontology:measurement_basis(spk_mktplace_be_t15, observed).
narrative_ontology:measurement(spk_mktplace_be_t20, speech_protection_kernel__marketplace_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement_basis(spk_mktplace_be_t20, observed).
narrative_ontology:measurement(spk_mktplace_be_t25, speech_protection_kernel__marketplace_reading, base_extractiveness, 25, 0.57).
narrative_ontology:measurement_basis(spk_mktplace_be_t25, observed).
narrative_ontology:measurement(spk_mktplace_be_t30, speech_protection_kernel__marketplace_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(spk_mktplace_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(spk_mktplace_su_t0, speech_protection_kernel__marketplace_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(spk_mktplace_su_t0, observed).
narrative_ontology:measurement(spk_mktplace_su_t5, speech_protection_kernel__marketplace_reading, suppression_requirement, 5, 0.43).
narrative_ontology:measurement_basis(spk_mktplace_su_t5, observed).
narrative_ontology:measurement(spk_mktplace_su_t10, speech_protection_kernel__marketplace_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement_basis(spk_mktplace_su_t10, observed).
narrative_ontology:measurement(spk_mktplace_su_t15, speech_protection_kernel__marketplace_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement_basis(spk_mktplace_su_t15, observed).
narrative_ontology:measurement(spk_mktplace_su_t20, speech_protection_kernel__marketplace_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement_basis(spk_mktplace_su_t20, observed).
narrative_ontology:measurement(spk_mktplace_su_t25, speech_protection_kernel__marketplace_reading, suppression_requirement, 25, 0.59).
narrative_ontology:measurement_basis(spk_mktplace_su_t25, observed).
narrative_ontology:measurement(spk_mktplace_su_t30, speech_protection_kernel__marketplace_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement_basis(spk_mktplace_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__marketplace_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__dignity_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__marketplace_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'free speech' conflates five structurally distinct claims with different justification bases, victim sets, and remedy legitimacies. This file authors the marketplace_reading alone (epsilon 0.58 over the standing protection arrangement as this reading assesses it). The upstream member is the absolutist_reading (highest empirical continuity with the doctrinal tradition); the marketplace reading sits mid-family, citing the absolutist settlement as background while supplying the instrumental warrant; the harm_threshold and dignity readings are downstream competitors whose remedies this reading's dominance renders procedurally illegitimate. Each family member links to the others via network.affects_constraints; no single story hedges epsilon across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
