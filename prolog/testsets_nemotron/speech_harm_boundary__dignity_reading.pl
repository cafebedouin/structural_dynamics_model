% ============================================================================
% CONSTRAINT STORY: speech_harm_boundary__dignity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_harm_boundary__dignity_reading, []).

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
 *   constraint_id: speech_harm_boundary__dignity_reading
 *   human_readable: Dignity-Based Speech Harm Boundary (Categorical Exclusion Reading)
 *   domain: constitutional_law/political_philosophy/communication_ethics
 *
 * SUMMARY:
 *   This constraint story instantiates the dignity_reading of the
 *   speech_harm_boundary kernel: speech protection is structurally
 *   subordinate to human dignity, and speech that denies personhood
 *   (Holocaust denial, hate speech targeting protected groups, group
 *   defamation) is categorically excluded from protection. The arrangement
 *   operates through constitutional provisions (German Basic Law Art. 1/5,
 *   South African Constitution §16(2), Canadian Charter §319, EU Framework
 *   Decision 2008/913/JHA) and platform governance regimes (NetzDG, DSA,
 *   Canadian Online Harms Act) that enforce categorical bans. The
 *   claimed_type is tangled_rope because the constraint performs a genuine
 *   coordination function (protecting the equal standing of dignity-bearing
 *   persons in public discourse) while simultaneously extracting heavily from
 *   speakers of dignity-violating speech through active suppression
 *   machinery. The engine will compute per-seat classifications from the
 *   structural data below.
 *
 * KEY AGENTS:
 *   - dignity_protected_groups: Primary beneficiaries (institutional/moderate/constrained) — gain equal discursive standing through categorical exclusion of personhood-denying speech
 *   - state_dignity_regulators: Agenda setters (institutional/generational/arbitrage) — administer the exclusion regime, define boundary of 'personhood-denying speech', wield enforcement
 *   - anti_hate_speech_ngos: Beneficiaries (organized/biographical/mobile) — advocacy infrastructure funded by and legitimated through the regime
 *   - dignity_violating_speakers: Primary victims (powerless/immediate/trapped) — Holocaust deniers, organized hate speakers, group defamers; face criminal prosecution, platform bans, professional exclusion
 *   - fringe_political_parties: Victims (moderate/biographical/constrained) — parties whose rhetoric brushes the boundary; face bans, surveillance, funding cuts
 *   - historical_revisionists: Victims (powerless/biographical/trapped) — academics/independent researchers whose work is classified as denialism; career destruction, publication bans
 *   - provocateur_speakers: Victims (moderate/immediate/mobile) — actors who test boundaries deliberately; platform bans, demonetization, legal jeopardy
 *   - absolutist_advocates: Excluded (organized/biographical/mobile) — civil liberties orgs, free speech absolutists; would argue for near-absolute protection but are structurally excluded from regime design
 *   - harm_balancing_scholars: Observers (analytical/generational/analytical) — proportionality theorists; analyze the regime but do not set its agenda
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_harm_boundary__dignity_reading, 0.78).
domain_priors:suppression_score(speech_harm_boundary__dignity_reading, 0.85).
domain_priors:theater_ratio(speech_harm_boundary__dignity_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(speech_harm_boundary__dignity_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_harm_boundary__dignity_reading, tangled_rope).
narrative_ontology:human_readable(speech_harm_boundary__dignity_reading, "Dignity-Based Speech Harm Boundary (Categorical Exclusion Reading)").
narrative_ontology:topic_domain(speech_harm_boundary__dignity_reading, "constitutional_law/political_philosophy/communication_ethics").

domain_priors:requires_active_enforcement(speech_harm_boundary__dignity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_harm_boundary__dignity_reading, '147edfdb-2c99-4a1e-b558-f3fdc095fe82').
narrative_ontology:cs_kernel_codification('147edfdb-2c99-4a1e-b558-f3fdc095fe82', formalized).
narrative_ontology:cs_authority_grounding('147edfdb-2c99-4a1e-b558-f3fdc095fe82', lineage).
narrative_ontology:cs_interpretation_layer_present('147edfdb-2c99-4a1e-b558-f3fdc095fe82').
narrative_ontology:cs_reading_relation('147edfdb-2c99-4a1e-b558-f3fdc095fe82', speech_harm_boundary__absolutist_reading, forecloses).
narrative_ontology:cs_reading_relation('147edfdb-2c99-4a1e-b558-f3fdc095fe82', speech_harm_boundary__harm_balancing_reading, coexists_with).
narrative_ontology:cs_axiom('147edfdb-2c99-4a1e-b558-f3fdc095fe82', foundational, human_dignity_apex_right).
narrative_ontology:cs_axiom_status(human_dignity_apex_right, holdable).
narrative_ontology:cs_axiom_grounding('147edfdb-2c99-4a1e-b558-f3fdc095fe82', human_dignity_apex_right, deontological).
narrative_ontology:cs_axiom('147edfdb-2c99-4a1e-b558-f3fdc095fe82', foundational, personhood_denying_speech_categorically_excluded).
narrative_ontology:cs_axiom_status(personhood_denying_speech_categorically_excluded, holdable).
narrative_ontology:cs_axiom_grounding('147edfdb-2c99-4a1e-b558-f3fdc095fe82', personhood_denying_speech_categorically_excluded, deontological).
narrative_ontology:cs_axiom('147edfdb-2c99-4a1e-b558-f3fdc095fe82', secondary, counterspeech_insufficient_for_dignity_harm).
narrative_ontology:cs_axiom_status(counterspeech_insufficient_for_dignity_harm, holdable).
narrative_ontology:cs_axiom_grounding('147edfdb-2c99-4a1e-b558-f3fdc095fe82', counterspeech_insufficient_for_dignity_harm, empirically_contingent).
narrative_ontology:cs_reference_frame('147edfdb-2c99-4a1e-b558-f3fdc095fe82', post_war_dignity_constitutionalism).
narrative_ontology:cs_drift_state('147edfdb-2c99-4a1e-b558-f3fdc095fe82', digital_platform_governance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('147edfdb-2c99-4a1e-b558-f3fdc095fe82', '2026-08-15T14:30:00Z').
narrative_ontology:cs_kernel_id(speech_harm_boundary__dignity_reading, speech_harm_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, dignity_protected_groups).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, state_dignity_regulators).
narrative_ontology:constraint_beneficiary(speech_harm_boundary__dignity_reading, anti_hate_speech_ngos).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, dignity_violating_speakers).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, fringe_political_parties).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, historical_revisionists).
narrative_ontology:constraint_victim(speech_harm_boundary__dignity_reading, provocateur_speakers).
narrative_ontology:constraint_vindicates(speech_harm_boundary__dignity_reading, human_dignity_as_constitutional_anchor).
narrative_ontology:constraint_vindicates(speech_harm_boundary__dignity_reading, group_defamation_as_personhood_denial).
narrative_ontology:constraint_vindicates(speech_harm_boundary__dignity_reading, speech_hierarchy_with_dignity_apex).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Groups protected by dignity-based speech bans (racial/ethnic/religious minorities, LGBTQ+ communities, Holocaust survivors and descendants). They gain equal discursive standing and protection from personhood-denying speech. Their exit is constrained: they cannot individually opt out of the protection, and their political organizing depends on the regime's continuation. They do not administer the regime.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, dignity_protected_groups, beneficiary,
    moderate, generational, constrained, national).

% Constitutional courts, prosecutors, media regulators (e.g., German BVerfG, Landesmedienanstalten; South African Equality Courts; Canadian Human Rights Tribunals; EU Commission under DSA). They define the boundary of 'personhood-denying speech', initiate enforcement, and expand the regime through precedent. They have arbitrage-grade exit: they can move between national/EU/international posts, and their institutional capital grows with the regime's scope.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, state_dignity_regulators, agenda_setter,
    institutional, generational, arbitrage, national).

% Organizations like Memorial Sites, ADL, SOS Racisme, HateAid. They receive state funding, platform reporting partnerships, and legislative consultation access because the regime exists. They advocate for boundary expansion. Their exit is mobile: they can pivot to other human rights work, but their current funding and relevance depend on the dignity-exclusion regime.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, anti_hate_speech_ngos, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(speech_harm_boundary__dignity_reading, anti_hate_speech_ngos, agenda_setter).

% Organized Holocaust deniers, neo-Nazi speakers, explicit hate speech organizers. They face criminal prosecution (German §130 StGB, French Gayssot Act, Canadian §319), platform bans, banking de-platforming, professional exclusion. Their exit is trapped: past speech creates permanent records; identity is fused with the targeted ideology; no realistic path to reintegration without public recantation under duress.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, dignity_violating_speakers, payer,
    powerless, immediate, trapped, national).

% Parties like AfD (Germany), RN (France, pre-dedemonization), Vox (Spain), Golden Dawn (Greece, banned). Their rhetoric brushes the dignity boundary. They face surveillance by domestic intelligence, funding cuts, candidate bans, party ban proceedings. Exit is constrained: they can moderate rhetoric (losing core base) or radicalize (triggering bans). Some successfully rebrand; most oscillate.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, fringe_political_parties, payer,
    moderate, biographical, constrained, national).

% Academics, independent researchers, publishers whose work on WWII/Holocaust/communist crimes is classified as denialism or relativization (e.g., Ernst Nolte, David Irving, revisionist historians in Eastern Europe). Career destruction, publication bans, conference exclusion, loss of archives access. Exit is trapped: professional identity is fused with the targeted scholarship; recantation ends the career; the label 'denier' is permanent.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, historical_revisionists, payer,
    powerless, biographical, trapped, national).

% Comedians, satirists, edgelords, performance artists who deliberately test the dignity boundary (e.g., Dieudonné, Count Dankula, various stand-up cases). They face platform bans, demonetization, venue cancellations, occasional prosecution. Exit is mobile: they can pivot platforms, change format, go underground, or rebrand — but each cycle reduces reach and revenue. Some monetize the martyrdom.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, provocateur_speakers, payer,
    moderate, immediate, mobile, global).

% Civil liberties organizations (ACLU Germany, Article 19, Reporters Without Borders national chapters, liberal legal scholars). They argue for near-absolute speech protection, proportionality, and counterspeech. They are structurally excluded from regime design: their testimony is heard but not determinative; their litigation loses at constitutional courts. They can mobilize public opinion and win occasional ECHR cases, but the regime's categorical logic is insulated from their challenges.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, absolutist_advocates, excluded,
    organized, biographical, mobile, national).

% Constitutional theorists, comparative law scholars, political philosophers working on proportionality doctrine (e.g., Alexy, Barak, Rivers, Canadian Oakes test lineage). They analyze the regime's structure, critique overbreadth, and propose balancing alternatives. They do not set agenda, collect rents, or bear costs — their seat is analytical.
narrative_ontology:constraint_stakeholder(speech_harm_boundary__dignity_reading, harm_balancing_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Secures the equal discursive standing of all persons by categorically excluding speech that denies the personhood/dignity of protected groups. Solves the coordination problem: without categorical exclusion, targeted groups face a 'marketplace of ideas' where their humanity is perpetually up for debate, undermining their capacity to participate as equals.
% TRANSFER_FUNCTION: Moves discursive space, legal safety, and institutional legitimacy from speakers of dignity-violating speech to dignity-protected groups and the state regulators who administer the exclusion. The transfer is coercive: criminal penalties, platform bans, professional sanctions enforce the transfer.
% ABSENT_VOICES: Absolutist free speech advocates (civil liberties orgs, liberal scholars) who would argue for counterspeech and proportionality over categorical bans. They are present in public discourse but excluded from the regime's constitutional logic — their position is treated as a category error (protecting the 'right to deny dignity') rather than a competing value. Also absent: future generations who would inherit a narrowed speech environment; their interests are not represented in the current regime design.
% DISAPPEARANCE_RATIONALE: If the categorical dignity exclusions vanished overnight: (1) Holocaust denial and organized hate speech would become legally protected in Germany, France, Austria, Canada, etc.; (2) platform governance regimes (NetzDG, DSA) would lose their core legal mandate for proactive takedowns; (3) equality bodies and hate speech NGOs would lose statutory funding and enforcement partnerships; (4) fringe parties would gain legal protection for rhetoric currently banned; (5) the constitutional order in Germany and South Africa would face a foundational crisis (Art. 1 GG / §10 SA Constitution). The discursive and legal world would rearrange profoundly.
% FOUNDING_PROBLEM: Post-1945: Preventing the discursive conditions that enabled genocide — the normalization of dehumanizing speech, the erosion of shared reality about targeted groups' humanity, the failure of liberal counterspeech to stop the progression from rhetoric to extermination. The Weimar experience demonstrated that procedural neutrality in the face of anti-democratic speech is suicide for democracy.
% FOUNDING_PROBLEM_CORROBORATION: Dignity_protected_groups and state_dignity_regulators attest the problem is live: rising antisemitism, anti-Muslim hatred, transphobic rhetoric, online radicalization pipelines — the Weimar conditions are reproducing digitally. Absolutist_advocates and harm_balancing_scholars attest the original problem (state-led extermination enabled by speech) is substantially solved: no Western state is on a genocide trajectory; the regime now targets speech that offends dignity but lacks exterminationist intent. Independent corroboration: Holocaust historians (Saul Friedländer, Christopher Browning) warn of trivialization but distinguish denialism from legitimate historiography; digital extremism researchers (Julia Ebner, ISD) document radicalization pathways but note categorical bans drive activity underground without reducing recruitment.
narrative_ontology:disappearance_verdict(speech_harm_boundary__dignity_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_harm_boundary__dignity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_harm_boundary__dignity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(speech_harm_boundary__dignity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_harm_boundary__dignity_reading, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_harm_boundary__dignity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(speech_harm_boundary__dignity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(speech_harm_boundary__dignity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is very high because the constraint categorically removes entire categories of speech from protection and imposes criminal/civil penalties — the transfer from speakers to protected groups (equal discursive standing) is large and coercive. Suppression (0.85) is very high because the regime depends on active enforcement: criminal laws, platform takedown mandates, regulatory fines, professional sanctions. Alternatives (counterspeech, civil remedy, proportionality balancing) are structurally suppressed — the categorical logic forbids them as insufficient. Theater ratio (0.22) is low-moderate: the dignity-protection function is genuine and actively enforced, but a growing share of enforcement targets borderline speech (ironic, academic, artistic) where the dignity threat is contested, creating performative overreach. Accessibility collapse (0.72) is high: once the categorical principle is accepted, proportionality balancing and content-neutral alternatives are logically foreclosed within the framework. Resistance (0.68) is substantial: absolutist and balancing readings persist as live alternatives, courts in some jurisdictions resist expansion, and platform governance creates friction.
 *
 * PERSPECTIVAL GAP:
 *   The engine will compute stark seat divergence: from dignity_protected_groups and state_dignity_regulators, the constraint computes as rope/scaffold (genuine coordination securing equal standing); from dignity_violating_speakers and historical_revisionists, it computes as snare (categorical extraction with trapped exit); from fringe_political_parties and provocateur_speakers, it computes as tangled_rope (coordination function acknowledged but extraction experienced as disproportionate); from absolutist_advocates, it computes as snare (pure extraction disguised as coordination). The authored claim (tangled_rope) reflects the structural asymmetry: real coordination function + real asymmetric extraction + active enforcement.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (dignity_protected_groups, state_dignity_regulators, anti_hate_speech_ngos) derive d near 0.0-0.2: the constraint subsidizes their discursive standing and institutional position. Victims (dignity_violating_speakers, historical_revisionists) derive d near 0.9-1.0: identity-locked exit (cannot change past speech, professional identity fused with targeted speech), trapped by criminal records and platform bans. Fringe_political_parties and provocateur_speakers derive d ~0.6-0.75: constrained exit (can moderate speech but lose base/brand), mobile enough to adapt but pay high cost. Absolutist_advocates are excluded (d undefined for constraint participation). Harm_balancing_scholars are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (post-WWII: preventing discourse conditions that enabled genocide) was live and the arrangement was a genuine scaffold/rope. The founding problem is now contested: dignity_protected_groups and regulators attest it remains live (rising hate speech, online radicalization); absolutist_advocates and harm_balancing_scholars attest it is substantially solved in its original form (no Weimar-like collapse imminent) and the regime now functions as mandate drift — categorical expansion beyond explicit denialism into structural discourse regulation. The constraint persists with high extraction and suppression because the agenda_setters (state regulators) benefit institutionally from the expanded mandate, while the victims lack coalition power to force sunset. This is mandatrophy: the coordination function has atrophied relative to the extraction function, but the constraint persists through institutional inertia and moral licensing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint one reading of a contested kernel (speech_harm_boundary), and what structural elements distinguish it from sibling readings?',
    'Comparative constraint analysis across the three declared readings: absolutist_reading, dignity_reading, harm_balancing_reading. Each reading instantiates a distinct constraint with its own ε, beneficiary/victim structure, and classification.',
    'Confirms this reading''s ε-invariance: its extractiveness (0.78) refers to the standing arrangement of categorical dignity-based exclusions, not to the absolutist or balancing arrangements. Sibling readings would author different ε values over the same referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Commitment that this is the dignity_reading of the speech_harm_boundary kernel, with forecloses/coexists_with/influences relations to sibling readings and distinct foundational axioms.').

omega_variable(
    dignity_vs_content_neutrality_boundary,
    'Where exactly does the dignity-based exclusion boundary lie — does it capture only explicit personhood denial (Holocaust denial, direct hate speech) or extend to speech that ''contributes to'' dignity erosion (microaggressions, systemic discourse patterns)?',
    'Case law trajectory analysis across jurisdictions with dignity-based frameworks (Germany, South Africa, Canada, EU): track whether courts expand categorical exclusion beyond explicit denialism into structural/contributory speech.',
    'If boundary expands, ε rises further (more speech categorically excluded) and victim set widens; if boundary holds at explicit denialism, ε stabilizes near current level. Determines whether this reading converges toward snare (expanding) or stabilizes as tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dignity_vs_content_neutrality_boundary, empirical, 'Scope creep risk in dignity-based categorical exclusion doctrines.').

omega_variable(
    enforcement_asymmetry_mechanism,
    'Is the high suppression (0.85) driven primarily by state enforcement machinery or by platform-level private governance operating under state pressure / regulatory threat?',
    'Institutional mapping of enforcement actions: proportion of removals/prosecutions initiated by state bodies vs. platforms'' ''voluntary'' compliance with dignity codes under regulatory frameworks (e.g. NetzDG, DSA, Canadian Online Harms Act).',
    'If primarily state-driven, suppression is structurally coherent with the reading''s categorical logic; if primarily private governance under threat, the constraint operates through a delegation layer that obscures state action — affects mandatrophy and theater assessment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_asymmetry_mechanism, empirical, 'State vs. private enforcement attribution in dignity-based speech regimes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_harm_boundary__dignity_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(speech_harm_boundary__dignity_reading_tr_t1945, speech_harm_boundary__dignity_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(speech_harm_boundary__dignity_reading_tr_t1970, speech_harm_boundary__dignity_reading, theater_ratio, 1970, 0.08).
narrative_ontology:measurement(speech_harm_boundary__dignity_reading_tr_t1990, speech_harm_boundary__dignity_reading, theater_ratio, 1990, 0.12).
narrative_ontology:measurement(speech_harm_boundary__dignity_reading_tr_t2005, speech_harm_boundary__dignity_reading, theater_ratio, 2005, 0.16).
narrative_ontology:measurement(speech_harm_boundary__dignity_reading_tr_t2015, speech_harm_boundary__dignity_reading, theater_ratio, 2015, 0.2).
narrative_ontology:measurement(speech_harm_boundary__dignity_reading_tr_t2025, speech_harm_boundary__dignity_reading, theater_ratio, 2025, 0.22).

% Extraction over time
narrative_ontology:measurement(speech_harm_boundary__dignity_reading_be_t1945, speech_harm_boundary__dignity_reading, base_extractiveness, 1945, 0.15).
narrative_ontology:measurement(speech_harm_boundary__dignity_reading_be_t1970, speech_harm_boundary__dignity_reading, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(speech_harm_boundary__dignity_reading_be_t1990, speech_harm_boundary__dignity_reading, base_extractiveness, 1990, 0.52).
narrative_ontology:measurement(speech_harm_boundary__dignity_reading_be_t2005, speech_harm_boundary__dignity_reading, base_extractiveness, 2005, 0.62).
narrative_ontology:measurement(speech_harm_boundary__dignity_reading_be_t2015, speech_harm_boundary__dignity_reading, base_extractiveness, 2015, 0.71).
narrative_ontology:measurement(speech_harm_boundary__dignity_reading_be_t2025, speech_harm_boundary__dignity_reading, base_extractiveness, 2025, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(speech_harm_boundary__dignity_reading_su_t1945, speech_harm_boundary__dignity_reading, suppression_requirement, 1945, 0.35).
narrative_ontology:measurement(speech_harm_boundary__dignity_reading_su_t1970, speech_harm_boundary__dignity_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(speech_harm_boundary__dignity_reading_su_t1990, speech_harm_boundary__dignity_reading, suppression_requirement, 1990, 0.68).
narrative_ontology:measurement(speech_harm_boundary__dignity_reading_su_t2005, speech_harm_boundary__dignity_reading, suppression_requirement, 2005, 0.75).
narrative_ontology:measurement(speech_harm_boundary__dignity_reading_su_t2015, speech_harm_boundary__dignity_reading, suppression_requirement, 2015, 0.81).
narrative_ontology:measurement(speech_harm_boundary__dignity_reading_su_t2025, speech_harm_boundary__dignity_reading, suppression_requirement, 2025, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_harm_boundary__dignity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(speech_harm_boundary__dignity_reading, 0.08).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, speech_harm_boundary__absolutist_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, speech_harm_boundary__harm_balancing_reading).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, platform_governance_dsa).
narrative_ontology:affects_constraint(speech_harm_boundary__dignity_reading, netzdg_enforcement_regime).

% DUAL FORMULATION NOTE:
% The speech_harm_boundary kernel decomposes into three constraint stories: absolutist_reading (near-absolute protection, low ε), dignity_reading (this file, categorical exclusion, high ε), harm_balancing_reading (proportionality, medium ε). The dignity_reading cites the absolutist_reading's failure to prevent dignity harm as evidence for categorical exclusion; the harm_balancing_reading cites the dignity_reading's overbreadth as evidence for proportionality. All three share the referent (the standing arrangement of speech-harm regulation) but author different ε and structural data.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_harm_boundary__dignity_reading, institutional, 0.15).
constraint_indexing:directionality_override(speech_harm_boundary__dignity_reading, powerless, 0.95).
constraint_indexing:directionality_override(speech_harm_boundary__dignity_reading, moderate, 0.7).
constraint_indexing:directionality_override(speech_harm_boundary__dignity_reading, organized, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
