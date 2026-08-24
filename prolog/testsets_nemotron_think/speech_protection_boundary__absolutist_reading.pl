% ============================================================================
% CONSTRAINT STORY: speech_protection_boundary__absolutist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   human_readable: Absolutist Speech Protection Boundary (Brandenburg Standard)
 *   domain: constitutional_law/political_philosophy/speech_regulation
 *
 * SUMMARY:
 *   The absolutist reading of the speech protection boundary instantiates the
 *   Brandenburg standard: speech is protected unless it is 'directed to
 *   inciting or producing imminent lawless action' and likely to produce such
 *   action. This reading maximizes the protected set, restricting the
 *   unprotected category to direct incitement of imminent violence. The
 *   doctrine emerged from Brandenburg v. Ohio (1969), reversing the 'clear
 *   and present danger' lineage (Schenck, Dennis) that had licensed
 *   suppression of leftist dissent. The absolutist reading presents itself as
 *   a constitutional mountain — a natural-law principle derived from the
 *   First Amendment's text and structure. However, its operation produces
 *   identifiable beneficiaries (speakers of extremist, hateful, and harassing
 *   speech) and identifiable victims (minoritized communities who bear the
 *   aggregate harm of protected hate speech as an externality). The claimed
 *   mountain classification diverges from the metric profile: extractiveness
 *   has risen from 0.35 to 0.62 over the interval as hate speech proliferates
 *   online and the harm borne by targeted groups compounds, while
 *   accessibility_collapse remains high (0.88) because alternative balancing
 *   frameworks are doctrinally excluded.
 *
 * KEY AGENTS:
 *   - extremist_speakers: Primary beneficiary (powerful/arbitrage) — gain maximal protection for hateful/harassing speech
 *   - minoritized_communities: Primary victim (powerless/trapped) — bear aggregate harm as externality with no doctrinal exit
 *   - civil_liberties_advocates: Secondary beneficiary (organized/mobile) — institutional mission aligned with absolutist doctrine
 *   - supreme_court: Agenda setter (institutional/analytical) — administers and interprets the standard
 *   - lower_courts: Observer (organized/analytical) — apply the test in individual cases
 *   - targeted_groups: Victim (powerless/constrained) — specific subgroups within minoritized communities directly threatened
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_boundary__absolutist_reading, 0.62).
domain_priors:suppression_score(speech_protection_boundary__absolutist_reading, 0.15).
domain_priors:theater_ratio(speech_protection_boundary__absolutist_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_boundary__absolutist_reading, mountain).
narrative_ontology:human_readable(speech_protection_boundary__absolutist_reading, "Absolutist Speech Protection Boundary (Brandenburg Standard)").
narrative_ontology:topic_domain(speech_protection_boundary__absolutist_reading, "constitutional_law/political_philosophy/speech_regulation").

domain_priors:emerges_naturally(speech_protection_boundary__absolutist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_boundary__absolutist_reading, 'd5b696d2-862a-4a62-a66c-c6763bca4efd').
narrative_ontology:cs_kernel_codification('d5b696d2-862a-4a62-a66c-c6763bca4efd', formalized).
narrative_ontology:cs_authority_grounding('d5b696d2-862a-4a62-a66c-c6763bca4efd', lineage).
narrative_ontology:cs_interpretation_layer_present('d5b696d2-862a-4a62-a66c-c6763bca4efd').
narrative_ontology:cs_reading_relation('d5b696d2-862a-4a62-a66c-c6763bca4efd', speech_protection_boundary__harm_limited_reading, coexists_with).
narrative_ontology:cs_reading_relation('d5b696d2-862a-4a62-a66c-c6763bca4efd', speech_protection_boundary__balancing_reading, coexists_with).
narrative_ontology:cs_axiom('d5b696d2-862a-4a62-a66c-c6763bca4efd', foundational, content_neutrality_principle).
narrative_ontology:cs_axiom_status(content_neutrality_principle, holdable).
narrative_ontology:cs_axiom_grounding('d5b696d2-862a-4a62-a66c-c6763bca4efd', content_neutrality_principle, deontological).
narrative_ontology:cs_axiom('d5b696d2-862a-4a62-a66c-c6763bca4efd', foundational, imminence_requirement).
narrative_ontology:cs_axiom_status(imminence_requirement, holdable).
narrative_ontology:cs_axiom_grounding('d5b696d2-862a-4a62-a66c-c6763bca4efd', imminence_requirement, conventional).
narrative_ontology:cs_reference_frame('d5b696d2-862a-4a62-a66c-c6763bca4efd', brandenburg_bright_line).
narrative_ontology:cs_drift_state('d5b696d2-862a-4a62-a66c-c6763bca4efd', contemporary_hate_speech_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d5b696d2-862a-4a62-a66c-c6763bca4efd', '').
narrative_ontology:cs_kernel_id(speech_protection_boundary__absolutist_reading, speech_protection_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, extremist_speakers).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, civil_liberties_advocates).
narrative_ontology:constraint_beneficiary(speech_protection_boundary__absolutist_reading, mainstream_media_institutions).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, minoritized_communities).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, targeted_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(speech_protection_boundary__absolutist_reading, civil_liberties_advocates).
narrative_ontology:constraint_vindicates(speech_protection_boundary__absolutist_reading, first_amendment_absolutism).
narrative_ontology:constraint_vindicates(speech_protection_boundary__absolutist_reading, content_neutrality_principle).
narrative_ontology:constraint_vindicates(speech_protection_boundary__absolutist_reading, marketplace_of_ideas_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Speakers of hateful, extremist, and harassing speech gain maximal constitutional protection. They can organize, recruit, and disseminate ideology with near-zero legal risk as long as they avoid explicit imminent incitement. Their exit options are arbitrage-grade: they can platform-hop, use coded language, and exploit jurisdictional gaps.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, extremist_speakers, beneficiary,
    powerful, biographical, arbitrage, national).

% Black, Brown, Jewish, Muslim, LGBTQ+, immigrant, and other minoritized communities bear the aggregate harm of protected hate speech: psychological trauma, dignitary injury, chilling effect on political participation, and stochastic violence inspired by protected rhetoric. They cannot exit the polity; doctrinal exit (advocating for harm-limited standards) is structurally blocked by the mountain claim. Their resistance is growing but institutionally marginalized.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, minoritized_communities, payer,
    powerless, generational, trapped, national).

% Organizations like ACLU, FIRE, and EFF build institutional mission and fundraising around absolutist defense. They benefit from the clear bright-line rule. They also bear costs: defending unpopular speakers damages coalitions with social justice movements, and the doctrine's application to right-wing hate speech creates internal tension. Exit is mobile — they could pivot to harm-limited advocacy but would lose institutional identity.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, civil_liberties_advocates, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(speech_protection_boundary__absolutist_reading, civil_liberties_advocates, payer).

% Legacy and digital media institutions benefit from maximal speech protection for newsgathering, commentary, and platform liability shields. They have arbitrage-grade exit: they can lobby for regulatory carve-outs, shift to subscription models, or relocate operations. Their benefit is structural and concentrated.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, mainstream_media_institutions, beneficiary,
    institutional, generational, arbitrage, national).

% The Court administers the Brandenburg test, defines 'imminence' and 'likelihood,' and polices the boundary. It is structurally insulated from both the benefit (it does not speak) and the harm (it is not targeted). Its interest is doctrinal coherence and institutional legitimacy. The Court's self-conception as neutral arbiter depends on the mountain claim.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, supreme_court, agenda_setter,
    institutional, generational, analytical, national).

% Apply the Brandenburg test in individual cases. They experience the constraint as a coordination standard: a clear rule that reduces decision costs. They have analytical exit — they can critique the doctrine in concurrences but must follow precedent. Their situation is closest to symmetric (d ~ 0.5).
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, lower_courts, observer,
    organized, biographical, analytical, regional).

% Specific subgroups within minoritized communities directly threatened by protected rhetoric: synagogue congregations, Black churches, transgender youth, immigrant neighborhoods. They bear concentrated harm with constrained exit — they can relocate or hide but at severe cost. Their voice is absent from the doctrinal conversation.
narrative_ontology:constraint_stakeholder(speech_protection_boundary__absolutist_reading, targeted_groups, payer,
    powerless, immediate, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, predictable, content-neutral boundary for speech protection that prevents government censorship of dissent, minimizes chilling effects on legitimate discourse, and reduces judicial decision costs through a bright-line rule.
% TRANSFER_FUNCTION: Transfers the burden of harm from speakers of extremist/hateful speech to the minoritized communities targeted by that speech. The constraint moves dignity, safety, and equal participation from targeted groups to the abstract collective benefit of 'uninhibited marketplace of ideas.'
% ABSENT_VOICES: Minoritized communities bearing the harm externality are structurally excluded from the doctrinal conversation. The Brandenburg test was forged in a case involving a KKK leader; the voices of those his rhetoric targeted were not part of the adjudication. Contemporary targeted groups (trans youth, Muslim communities, Black voters facing stochastic terrorism) have no seat at the Supreme Court and limited access to the amicus process.
% DISAPPEARANCE_RATIONALE: If the Brandenburg standard vanished overnight, U.S. speech doctrine would immediately revert to a balancing or harm-limited approach (as exists in every other constitutional democracy). Hate speech laws would be enacted within months; platforms would face liability for extremist content; the coordinate system for First Amendment litigation would collapse and reorganize around harm-prevention.
% FOUNDING_PROBLEM: Preventing government suppression of political dissent, particularly leftist and labor organizing, under vague 'clear and present danger' and 'bad tendency' standards that licensed prosecution of speakers for advocacy of abstract ideas.
% FOUNDING_PROBLEM_CORROBORATION: Historical record corroborates: Brandenburg overturned a KKK leader's conviction under an Ohio criminal syndicalism law; the ACLU's amicus brief framed the case as protecting 'advocacy of ideas' from government overreach. However, civil rights organizations (NAACP Legal Defense Fund, Southern Poverty Law Center) and critical race theorists attest that the founding problem has mutated: the doctrine now primarily shields right-wing hate speech targeting the communities the civil rights movement sought to protect. No corroborating source outside the absolutist-beneficiary set attests that the original problem remains the dominant one.
narrative_ontology:disappearance_verdict(speech_protection_boundary__absolutist_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_boundary__absolutist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_boundary__absolutist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(speech_protection_boundary__absolutist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_boundary__absolutist_reading, 0.62, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_boundary__absolutist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_protection_boundary__absolutist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, ExtMetricName, E),
    domain_priors:suppression_score(speech_protection_boundary__absolutist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(speech_protection_boundary__absolutist_reading),
    narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(speech_protection_boundary__absolutist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(speech_protection_boundary__absolutist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) reflects the rising harm burden on minoritized communities as the protected set expands to cover online hate speech, harassment campaigns, and stochastic terrorism rhetoric. The constraint extracts tolerance-of-harm from those least able to exit. Suppression (0.15) is low because the constraint itself is anti-suppressive — it restricts government action — but the suppression_requirement measurements show a slight upward trend as enforcement of the boundary against government overreach requires more active judicial policing. Theater ratio (0.18) is low and stable: the doctrine is genuinely applied, not performatively maintained. Accessibility_collapse (0.88) is high: balancing and harm-limited alternatives are doctrinally foreclosed within the absolutist reading's framework. Resistance (0.12) is low from institutional actors but rising from minoritized communities (captured in omega variables).
 *
 * PERSPECTIVAL GAP:
 *   From the Court's analytical seat, the constraint appears as a mountain: clear rule, minimal suppression, high accessibility_collapse (alternatives are legally foreclosed). From minoritized communities' payer seat, the constraint operates as extraction: they pay the cost of the coordination function (clear speech rules) without receiving its benefit (protection from harm). The engine computes this per-seat divergence from the structural data; the authored metrics describe the aggregate operation while the stakeholder seats capture the distributional asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Extremist speakers are structural beneficiaries (d ~ 0.1): they collect the full protective benefit with near-zero cost. Civil liberties advocates are secondary beneficiaries (d ~ 0.2): institutional mission aligns, but they bear reputational costs for defending unpopular speakers. Minoritized communities are structural victims (d ~ 0.85): they bear the harm externality with trapped exit options — they cannot leave the polity, and doctrinal exit (switching to a harm-limited reading) is blocked by the constraint's own mountain claim. The Court sits near analytical (d ~ 0.5): it administers the standard but is structurally insulated from both benefit and harm. The directionality derivation from beneficiary/victim declarations plus exit options produces the expected divergence: the same constraint computes as mountain from the Court's seat and as extraction from the minoritized-community seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing government suppression of dissent) was live in 1969. Its status is now contested: the original threat (government prosecuting leftist organizers) has receded, but the doctrine now primarily protects right-wing hate speech targeting minoritized communities. The arrangement persists because the institutional beneficiaries (civil liberties organizations, mainstream media) have not pivoted, and the Court's self-conception depends on the mountain claim. This is a classic mandatrophy pattern: the coordination function (preventing government censorship) remains partially live, but the extraction function (externalizing harm to minoritized communities) has grown. The constraint is not a pure snare because the coordination function is genuine; it is a false-summit mountain candidate where the natural-law presentation obscures the beneficiary/victim structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine natural-law mountain, or a constructed doctrine that benefits identifiable speakers while externalizing harm to minoritized communities?',
    'Compare the constraint''s operation across sibling readings: if the absolutist reading''s beneficiaries (extremist speakers) and victims (minoritized communities) are structurally necessary to its maintenance, the mountain claim is a false summit. Empirical test: track whether doctrine shifts when beneficiary composition changes (e.g., from leftist to right-wing speakers).',
    'If false summit, reclassification to tangled_rope via FSM trigger; the constraint would be recognized as coordinating speech protection while extracting harm-tolerance from minoritized communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Natural-law vs. constructed-beneficiary ambiguity for absolutist speech doctrine.').

omega_variable(
    harm_externality_structure,
    'Is the aggregate harm borne by minoritized communities a genuine externality of the coordination function, or is the constraint''s coherence dependent on that harm being borne by those specific communities?',
    'Counterfactual: if minoritized communities gained effective political power to internalize the harm (e.g., through hate speech laws), would the absolutist reading maintain its form or mutate? Historical comparison: post-WWII European democracies adopted harm-limited readings without collapsing speech protection entirely.',
    'If harm-bearing is structurally necessary, the constraint is tangled_rope (coordination + asymmetric extraction). If genuinely external, mountain classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(harm_externality_structure, empirical, 'Whether minoritized-community harm is structural extraction or incidental externality.').

omega_variable(
    brandenburg_neutrality_contest,
    'Does the Brandenburg test operate neutrally across speaker identities, or does its application track the power of the speaker?',
    'Empirical study of Brandenburg application rates by speaker ideology, race, and institutional affiliation. Compare incitement prosecutions of Black Panthers vs. KKK vs. contemporary far-right actors.',
    'If application tracks speaker power, the constraint''s claimed neutrality is a cover story; effective extraction targets powerless speakers while powerful speakers capture the benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(brandenburg_neutrality_contest, empirical, 'Neutral application vs. power-tracking application of the imminence standard.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_boundary__absolutist_reading, 1969, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(speech_protection_boundary__absolutist_reading_tr_t1969, speech_protection_boundary__absolutist_reading, theater_ratio, 1969, 0.12).
narrative_ontology:measurement(speech_protection_boundary__absolutist_reading_tr_t1980, speech_protection_boundary__absolutist_reading, theater_ratio, 1980, 0.14).
narrative_ontology:measurement(speech_protection_boundary__absolutist_reading_tr_t1995, speech_protection_boundary__absolutist_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(speech_protection_boundary__absolutist_reading_tr_t2005, speech_protection_boundary__absolutist_reading, theater_ratio, 2005, 0.16).
narrative_ontology:measurement(speech_protection_boundary__absolutist_reading_tr_t2015, speech_protection_boundary__absolutist_reading, theater_ratio, 2015, 0.17).
narrative_ontology:measurement(speech_protection_boundary__absolutist_reading_tr_t2025, speech_protection_boundary__absolutist_reading, theater_ratio, 2025, 0.18).

% Extraction over time
narrative_ontology:measurement(speech_protection_boundary__absolutist_reading_be_t1969, speech_protection_boundary__absolutist_reading, base_extractiveness, 1969, 0.35).
narrative_ontology:measurement(speech_protection_boundary__absolutist_reading_be_t1980, speech_protection_boundary__absolutist_reading, base_extractiveness, 1980, 0.42).
narrative_ontology:measurement(speech_protection_boundary__absolutist_reading_be_t1995, speech_protection_boundary__absolutist_reading, base_extractiveness, 1995, 0.48).
narrative_ontology:measurement(speech_protection_boundary__absolutist_reading_be_t2005, speech_protection_boundary__absolutist_reading, base_extractiveness, 2005, 0.53).
narrative_ontology:measurement(speech_protection_boundary__absolutist_reading_be_t2015, speech_protection_boundary__absolutist_reading, base_extractiveness, 2015, 0.58).
narrative_ontology:measurement(speech_protection_boundary__absolutist_reading_be_t2025, speech_protection_boundary__absolutist_reading, base_extractiveness, 2025, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(speech_protection_boundary__absolutist_reading_su_t1969, speech_protection_boundary__absolutist_reading, suppression_requirement, 1969, 0.1).
narrative_ontology:measurement(speech_protection_boundary__absolutist_reading_su_t1980, speech_protection_boundary__absolutist_reading, suppression_requirement, 1980, 0.12).
narrative_ontology:measurement(speech_protection_boundary__absolutist_reading_su_t1995, speech_protection_boundary__absolutist_reading, suppression_requirement, 1995, 0.13).
narrative_ontology:measurement(speech_protection_boundary__absolutist_reading_su_t2005, speech_protection_boundary__absolutist_reading, suppression_requirement, 2005, 0.14).
narrative_ontology:measurement(speech_protection_boundary__absolutist_reading_su_t2015, speech_protection_boundary__absolutist_reading, suppression_requirement, 2015, 0.15).
narrative_ontology:measurement(speech_protection_boundary__absolutist_reading_su_t2025, speech_protection_boundary__absolutist_reading, suppression_requirement, 2025, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_boundary__absolutist_reading, information_standard).
narrative_ontology:boltzmann_floor_override(speech_protection_boundary__absolutist_reading, 0.02).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, speech_protection_boundary__harm_limited_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, speech_protection_boundary__balancing_reading).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, hate_speech_regulation).
narrative_ontology:affects_constraint(speech_protection_boundary__absolutist_reading, campaign_finance_speech_boundary).

% DUAL FORMULATION NOTE:
% The speech_protection_boundary kernel decomposes into three constraint stories: absolutist_reading (this file), harm_limited_reading, and balancing_reading. Each has distinct ε, beneficiary/victim structures, and claimed types. The absolutist reading claims mountain with ε=0.62; the harm-limited reading claims tangled_rope with higher ε; the balancing reading claims scaffold or tangled_rope. They are linked via affects_constraints because the absolutist reading's doctrinal dominance forecloses the sibling readings' adoption in U.S. courts, while the sibling readings' existence in comparative law and academic discourse creates revival_pressure on the absolutist reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_boundary__absolutist_reading, institutional, 0.45).
constraint_indexing:directionality_override(speech_protection_boundary__absolutist_reading, powerless, 0.88).
constraint_indexing:directionality_override(speech_protection_boundary__absolutist_reading, organized, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
