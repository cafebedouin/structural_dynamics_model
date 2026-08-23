% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__dignity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__dignity_reading, []).

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
 *   constraint_id: speech_protection_kernel__dignity_reading
 *   human_readable: Conditional Speech Protection Regime (Equal-Dignity Reading)
 *   domain: constitutional law/political philosophy/communication rights
 *
 * SUMMARY:
 *   In jurisdictions adopting the dignity reading, constitutional and
 *   statutory speech protection is conditioned on expression not functioning
 *   as structural subordination of identifiable groups: hate speech and group
 *   libel fall outside the protected class, and equality bodies adjudicate
 *   the boundary. The epsilon referent is the standing arrangement under
 *   contest — the conditional-protection regime as actually administered —
 *   assessed by this reading's own lights, which count the restriction of
 *   subordinating speech as largely legitimate and therefore locate residual
 *   extraction in adjudicative discretion, chill spillover onto borderline
 *   speech, and adjudicative-apparatus growth rather than in the core
 *   restriction itself. This story is ONE reading of the
 *   speech_protection_kernel; the absolutist, harm_threshold, marketplace,
 *   and democratic_participation readings are separate constraints (separate
 *   files) with their own epsilon values, victim sets, and classifications,
 *   and are not averaged or hedged into this one.
 *
 * KEY AGENTS:
 *   - members_of_targeted_groups: Primary beneficiary (organized/constrained) — assured civic standing flows to them through the adjudicative process
 *   - equality_law_apparatus: Agenda-setting administrator (institutional/constrained) — adjudicates the subordination boundary and accrues jurisdiction, caseload, and staffing with each ruling
 *   - group_libelers_and_extremist_speakers: Primary payer (powerless/identity_locked) — bear fines, takedowns, and prosecution; ideologically fused with the restricted message
 *   - chilled_borderline_speakers: Secondary payer (moderate/constrained) — bear anticipatory self-editing costs without ever reaching an adjudicator
 *   - content_platforms: Institutional payer (powerful/arbitrage) — bear compliance and policing costs, partially offset by cross-border geofencing
 *   - democratic_legislators: Founding agenda-setter (institutional/mobile) — retain amendment power but face coalition costs in using it
 *   - free_expression_advocates: Organized opposing payer (organized/mobile) — absorb litigation and agenda costs of resisting category expansion
 *   - dissenting_minority_members: Excluded voice (powerless/trapped) — protected-class members who reject the protection and cannot opt out
 *   - comparative_constitutional_scholars: Analytical observer (analytical/analytical) — sees the full cross-jurisdictional structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__dignity_reading, 0.44).
domain_priors:suppression_score(speech_protection_kernel__dignity_reading, 0.58).
domain_priors:theater_ratio(speech_protection_kernel__dignity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(speech_protection_kernel__dignity_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__dignity_reading, "Conditional Speech Protection Regime (Equal-Dignity Reading)").
narrative_ontology:topic_domain(speech_protection_kernel__dignity_reading, "constitutional law/political philosophy/communication rights").

domain_priors:requires_active_enforcement(speech_protection_kernel__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__dignity_reading, 'e75fb42f-792a-4499-9d5b-72789c722b1d').
narrative_ontology:cs_kernel_codification('e75fb42f-792a-4499-9d5b-72789c722b1d', fixed_text).
narrative_ontology:cs_authority_grounding('e75fb42f-792a-4499-9d5b-72789c722b1d', lineage).
narrative_ontology:cs_interpretation_layer_present('e75fb42f-792a-4499-9d5b-72789c722b1d').
narrative_ontology:cs_reading_relation('e75fb42f-792a-4499-9d5b-72789c722b1d', speech_protection_kernel__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('e75fb42f-792a-4499-9d5b-72789c722b1d', speech_protection_kernel__harm_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('e75fb42f-792a-4499-9d5b-72789c722b1d', speech_protection_kernel__marketplace_reading, forecloses).
narrative_ontology:cs_reading_relation('e75fb42f-792a-4499-9d5b-72789c722b1d', speech_protection_kernel__democratic_participation_reading, coexists_with).
narrative_ontology:cs_axiom('e75fb42f-792a-4499-9d5b-72789c722b1d', foundational, group_harm_distinct_from_individual_harm).
narrative_ontology:cs_axiom_status(group_harm_distinct_from_individual_harm, holdable).
narrative_ontology:cs_axiom_grounding('e75fb42f-792a-4499-9d5b-72789c722b1d', group_harm_distinct_from_individual_harm, deontological).
narrative_ontology:cs_axiom('e75fb42f-792a-4499-9d5b-72789c722b1d', foundational, equal_civic_standing_preconditions_participation).
narrative_ontology:cs_axiom_status(equal_civic_standing_preconditions_participation, holdable).
narrative_ontology:cs_axiom_grounding('e75fb42f-792a-4499-9d5b-72789c722b1d', equal_civic_standing_preconditions_participation, deontological).
narrative_ontology:cs_axiom('e75fb42f-792a-4499-9d5b-72789c722b1d', secondary, protection_conditional_on_non_subordination).
narrative_ontology:cs_axiom_status(protection_conditional_on_non_subordination, holdable).
narrative_ontology:cs_axiom_grounding('e75fb42f-792a-4499-9d5b-72789c722b1d', protection_conditional_on_non_subordination, conventional).
narrative_ontology:cs_reference_frame('e75fb42f-792a-4499-9d5b-72789c722b1d', equal_dignity_precondition_framework).
narrative_ontology:cs_drift_state('e75fb42f-792a-4499-9d5b-72789c722b1d', contemporary_platform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e75fb42f-792a-4499-9d5b-72789c722b1d', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__dignity_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, members_of_targeted_groups).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__dignity_reading, general_public).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, group_libelers_and_extremist_speakers).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, chilled_borderline_speakers).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, content_platforms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, general_public).
narrative_ontology:constraint_victim(speech_protection_kernel__dignity_reading, free_expression_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live under a legal order in which expression depicting their group as unfit for membership can be brought before equality bodies and formally answered. They file complaints, testify, and rely on the resulting rulings to keep their standing in workplaces, housing markets, and public debate from being openly contested. Leaving the jurisdiction that protects them would mean losing home, work, and community, so most stay and work through the system.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, members_of_targeted_groups, beneficiary,
    organized, biographical, constrained, national).

% Inhabit the resulting discourse environment: they read, argue, and joke under rules that remove the most subordinating material from circulation. They gain a public square in which no group's belonging is routinely voted down, and pay in the form of softened satire, policed comment sections, and occasional news stories about prosecuted posts.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, general_public, beneficiary,
    moderate, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_kernel__dignity_reading, general_public, payer).

% Publish material portraying ethnic, religious, or migrant groups as dangerous, dishonest, or unworthy of membership. When adjudicators classify their output as subordinating, they face fines, takedowns, or prosecution. They rarely accept the classification and typically see themselves as truth-tellers rather than subordinators; abandoning the message would dissolve the movements and identities built around it.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, group_libelers_and_extremist_speakers, payer,
    powerless, biographical, identity_locked, national).

% Satirists, polemicists, academics, and performers whose work brushes against sensitive group topics. They cannot reliably predict where adjudicators will draw the line between provocative and subordinating, so many soften or abandon projects near the boundary. Their realistic options are self-editing, changing subjects, or relocating their audience to other jurisdictions.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, chilled_borderline_speakers, payer,
    moderate, biographical, constrained, national).

% Operate global hosting services for user expression. They must apply removal rules derived from dignity-based statutes in each jurisdiction they serve, building policy teams and review pipelines to comply. Operating across borders lets them geofence stricter rules to stricter countries and route disputed content elsewhere, which blunts but does not eliminate their exposure.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, content_platforms, payer,
    powerful, biographical, arbitrage, global).

% Human rights commissions, equality tribunals, and courts that receive complaints, decide what counts as subordinating expression, and issue binding rulings. Each ruling extends or refines their jurisdiction, and budgets and staffing grow with caseload. They cannot step outside the adjudicative role their statutes assign them.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, equality_law_apparatus, agenda_setter,
    institutional, generational, constrained, national).

% Enacted the conditional-protection statutes and retain formal power to widen, narrow, or repeal them. Amendment requires assembling coalitions across parties that fear being cast as either soft on vilification or hostile to free expression, so most settle for periodic review rather than structural change.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, democratic_legislators, agenda_setter,
    institutional, biographical, mobile, national).

% Civil liberties organizations and legal scholars campaigning against expansion of restricted categories. They absorb litigation costs, divert organizational agendas from other civil-liberties work, and publish sustained critique. They can redirect attention to other issues or jurisdictions, and periodically do.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, free_expression_advocates, payer,
    organized, biographical, mobile, continental).

% Members of the very groups the statutes protect who reject the protection: some want to reclaim stigmatizing labels, some find the complaint machinery stigmatizing, some simply disagree with the framing. The statutes address them as a class whether or not they consent, and no procedure exists for opting out of being a protected-class member.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, dissenting_minority_members, excluded,
    powerless, biographical, trapped, national).

% Track how different jurisdictions draw the line between protected and restricted expression, publish comparisons, and advise reformers on all sides. They hold no stake in any single regime and can adopt any jurisdiction's framework as their object of study.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__dignity_reading, comparative_constitutional_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(speech_protection_kernel__dignity_reading, equality_law_apparatus).
narrative_ontology:fixing_cost_class(speech_protection_kernel__dignity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a shared public discourse in which membership in the political community is not contested through the medium of expression: by removing speech that functions as structural subordination from the protected class, the arrangement keeps one class of participants' equal standing from being publicly voted down, addressing the collective problem that a discourse commons open to group-deletion rhetoric degrades the standing of entire groups at once.
% TRANSFER_FUNCTION: Moves decision-rights over public expression from speakers and platforms to equality adjudicative bodies; moves expressive liberty and compliance costs from speakers classified as subordinating and from platforms that must police them; delivers assured civic standing to members of targeted groups.
% ABSENT_VOICES: Dissenting members of the protected groups themselves are structurally absent: statutes address them as a class, and no procedure solicits their consent or records their objection inside the adjudicative conversation. Speakers governed by absolutist-jurisdiction regimes are also absent from the boundary-setting conversation that defines what counts as subordinating.
% DISAPPEARANCE_RATIONALE: Overnight repeal would return group-vilifying material to full circulation, reopen the standing contests that rulings had closed, strand complaint machinery and its accumulated caseload, and force platforms to re-derive policy from scratch; protected-group advocacy networks, tribunal staffing, and platform policy teams would all reorganize around the change.
% FOUNDING_PROBLEM: Mid-twentieth-century constitutional design confronted the documented sequence in which unrestricted group vilification in public discourse preceded and enabled the exclusion and persecution of minority groups; the founding problem was how to keep public discourse from serving as an instrument of group subordination without abolishing free discourse itself.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship on the vilification-persecution sequence attests the founding problem from outside the benefiting coalition, and the reading's own opponents corroborate it as well: free-expression advocates and absolutist jurists dispute the remedy while conceding in their own analyses that the founding harms were real. No corroborating source attests that the problem is solved; all treat it as ongoing.
narrative_ontology:disappearance_verdict(speech_protection_kernel__dignity_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__dignity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__dignity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_kernel__dignity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__dignity_reading, 0.44, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__dignity_reading_tests).
:- end_tests(speech_protection_kernel__dignity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.44 at interval end) because the regime genuinely transfers decision-rights and expressive liberty, but the dignity reading's own assessment bounds the core restriction as justified, leaving residual extraction in classification error, chill spillover, and apparatus growth. Suppression (0.58) is authored as a RAW structural property and is deliberately NOT scaled by power or scope — the engine owns that arithmetic; the scalar reflects sanctions, takedowns, and chill, discounted partially by surviving alternatives (bounded counter-speech, jurisdiction shopping). Theater ratio (0.22) is low-moderate: adjudication is functionally real, with a growing symbolic-compliance layer (corporate statements, training regimes, ritual condemnations) riding on top. Accessibility collapse (0.38) is low for a governance constraint because alternatives persist once the rule is understood. Resistance (0.52) is substantial: organized free-expression campaigns, juristic dissent, and platform friction. The claim is authored independently of the metrics: tangled_rope because the arrangement possesses a genuine, externally corroborated coordination function (equal-standing discourse maintenance), an asymmetric payment structure (speakers and platforms pay; the apparatus accrues), and active enforcement dependence. The temporal series run on ONE shared seven-point grid with all three metrics authored at every point; suppression_requirement is tracked because the narrative specifically traces enforcement-capacity buildout (commission mandates, tribunal staffing, platform policy teams maturing over the interval), not merely shifting extraction. Trajectories are monotonic, not cyclical: enforcement maturation and category expansion proceed without observed oscillation, so no intermittent-reinforcement mechanism is posited.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the equality_law_apparatus seat the arrangement is a jurisdiction it administers and grows with; from the payer seats (libelers, chilled speakers, platforms) the same structure operates as restriction and compliance burden; from the members_of_targeted_groups seat it operates as protection; from the dissenting_minority_members seat it operates as protection imposed without consent. The sharpest same-level lateral contrast is members_of_targeted_groups versus dissenting_minority_members: identical nominal class position, opposite structural relationships, differentiated by consent and by exit (constrained versus trapped). Inter-institutionally, democratic_legislators (mobile, retaining amendment power) sit differently from equality_law_apparatus (constrained, growing with caseload) and content_platforms (arbitrage-capable across borders) despite overlapping institutional standing. Identity-lock dynamics: the libeler seat's identity_locked exit is ideological identity fusion — the restricted message constitutes the movement's self-concept, so exit would dissolve the identity rather than merely forfeit a channel; if that fusion broke, the seat's effective extraction would drop toward that of an ordinary regulated speaker.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for members_of_targeted_groups and (via the dual role) near-symmetric positioning for general_public. Victim declarations drive high directionality for the three payer seats, amplified toward the full-target end for group_libelers_and_extremist_speakers by their identity_locked exit, and damped for content_platforms by arbitrage-grade exit. No directionality_overrides are authored: the derivation chain captures every seat except equality_law_apparatus, whose benefit (jurisdiction and caseload accrual) is real but instrumental-administrative; listing it under beneficiaries would misdeclare its position, and a power-atom override keyed to 'institutional' would also strike democratic_legislators, which shares that atom with a different structural relationship. The apparatus seat therefore rides the canonical fallback, and the limitation is recorded here rather than papered over with a blunt override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem remains live (vilification persists, and no corroborating source — including the reading's opponents — attests otherwise), so no mandatrophy resolution is declared. The classification prevents symmetrical mislabeling: reading only the payment half would render the regime a censorship machine (snare misread); reading only the coordination half would render it pure protection (rope misread). Tangled_rope holds both halves together. The receipt surface records that the adjudicative apparatus is the seat the regime's gains demonstrably accrue to (caseload, jurisdiction, staffing) — a partial, instrumental accrual rather than full capture, since the regime's persistence rests on broad normative support from protected communities and egalitarian coalitions rather than on apparatus self-interest; this is why the claim remains tangled_rope despite a named gain seat. Fixing is prohibitive for the seats that could fix it: repeal requires cross-party coalitions, untangles treaty commitments, and strands dependent machinery, relative to a benefit of fixing that the fixing seats themselves do not collect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates only the dignity_reading of speech_protection_kernel; how much of its classification is indexical to that reading rather than to the underlying constitutional text-family the readings share?',
    'Generate and compare the four sibling-reading stories (absolutist, harm_threshold, marketplace, democratic_participation) under matched structural data; divergent computed types across readings locate the indexical component.',
    'If classification diverges sharply across readings, the corpus must treat speech-protection classification as reading-indexed; merging readings into one story would fabricate an observer-independent epsilon that violates epsilon-invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one reading of a five-reading kernel; sibling readings are separate constraints with their own epsilon and victim sets.').

omega_variable(
    subordination_boundary_discretion,
    'Does adjudicative practice track genuine structural subordination, or does the subordination boundary drift toward institutional convenience and category creep?',
    'Audit tribunal and commission rulings against independent measures of subordination effect (status-harm studies, targeted-group incident reporting); measure classification-error rates and scope expansion over time.',
    'Confirmed drift raises effective extraction on borderline speakers and trends the computed type toward snare; a tight boundary supports the tangled_rope claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_boundary_discretion, empirical, 'Whether the adjudicated subordination boundary tracks the phenomenon or the institution''s convenience.').

omega_variable(
    protection_consent_asymmetry,
    'Is protection that binds non-consenting members of targeted groups constitutive of the group''s interest, or an imposition that extracts acquiescence from a dissenting subset of the protected class?',
    'Survey and deliberative polling within protected populations; track opt-out attempts, their frequency, and how institutions treat them.',
    'If dissent is substantial, the beneficiary seat splits and effective extraction on the dissenting subset rises; the uniform-beneficiary declaration would need subdivision into consenting and non-consenting strata.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protection_consent_asymmetry, preference, 'Consent asymmetry inside the protected class: uniform beneficiary declaration versus a dissenting protected subset.').

omega_variable(
    chill_population_magnitude,
    'How large is the population of borderline speakers who self-edit without ever reaching an adjudicator, and does the victims declaration materially understate the payer set?',
    'Writer, artist, comedian, and academic surveys on self-censorship near group-topic boundaries; platform creator-attrition data around moderation expansions.',
    'A large chilled population raises measured extraction and suppression beyond the authored scalars and strengthens the payer side of the asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(chill_population_magnitude, empirical, 'Size of the anticipatory-self-censorship population relative to adjudicated cases.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__dignity_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(spk_dignity_tr_t0, speech_protection_kernel__dignity_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(spk_dignity_tr_t10, speech_protection_kernel__dignity_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(spk_dignity_tr_t20, speech_protection_kernel__dignity_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(spk_dignity_tr_t30, speech_protection_kernel__dignity_reading, theater_ratio, 30, 0.15).
narrative_ontology:measurement(spk_dignity_tr_t40, speech_protection_kernel__dignity_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(spk_dignity_tr_t50, speech_protection_kernel__dignity_reading, theater_ratio, 50, 0.2).
narrative_ontology:measurement(spk_dignity_tr_t60, speech_protection_kernel__dignity_reading, theater_ratio, 60, 0.22).

% Extraction over time
narrative_ontology:measurement(spk_dignity_be_t0, speech_protection_kernel__dignity_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(spk_dignity_be_t10, speech_protection_kernel__dignity_reading, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(spk_dignity_be_t20, speech_protection_kernel__dignity_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(spk_dignity_be_t30, speech_protection_kernel__dignity_reading, base_extractiveness, 30, 0.39).
narrative_ontology:measurement(spk_dignity_be_t40, speech_protection_kernel__dignity_reading, base_extractiveness, 40, 0.41).
narrative_ontology:measurement(spk_dignity_be_t50, speech_protection_kernel__dignity_reading, base_extractiveness, 50, 0.43).
narrative_ontology:measurement(spk_dignity_be_t60, speech_protection_kernel__dignity_reading, base_extractiveness, 60, 0.44).

% Suppression requirement over time
narrative_ontology:measurement(spk_dignity_su_t0, speech_protection_kernel__dignity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(spk_dignity_su_t10, speech_protection_kernel__dignity_reading, suppression_requirement, 10, 0.4).
narrative_ontology:measurement(spk_dignity_su_t20, speech_protection_kernel__dignity_reading, suppression_requirement, 20, 0.46).
narrative_ontology:measurement(spk_dignity_su_t30, speech_protection_kernel__dignity_reading, suppression_requirement, 30, 0.51).
narrative_ontology:measurement(spk_dignity_su_t40, speech_protection_kernel__dignity_reading, suppression_requirement, 40, 0.55).
narrative_ontology:measurement(spk_dignity_su_t50, speech_protection_kernel__dignity_reading, suppression_requirement, 50, 0.57).
narrative_ontology:measurement(spk_dignity_su_t60, speech_protection_kernel__dignity_reading, suppression_requirement, 60, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__dignity_reading, identity_coordination).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__dignity_reading, speech_protection_kernel__democratic_participation_reading).

% DUAL FORMULATION NOTE:
% Colloquial 'speech protection' decomposes, per epsilon-invariance, into five structurally distinct constraints — one per reading of the kernel. This file is the dignity_reading instantiation: protection conditional on non-subordination, with group harm treated as distinct from individual harm. Its epsilon (0.44) reflects the conditional regime's own mix of coordination and payment; the absolutist sibling authors a near-categorical shield, the harm_threshold sibling a demonstrability-gated regime, the marketplace sibling a counter-speech regime, the democratic_participation sibling a function-weighted regime. The absolutist reading (oldest codification) supplies the fixed texts all readings interpret; the dignity reading exerts downstream pressure on platform-moderation practice in adopting jurisdictions. Family links run through network.affects_constraints in every member file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
