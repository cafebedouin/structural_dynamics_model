% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__sovereignty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__sovereignty_primacy_reading, []).

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
 *   constraint_id: one_country_two_systems_framework__sovereignty_primacy_reading
 *   human_readable: One Country, Two Systems — Sovereignty Primacy Reading (Security Override Regime)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   Since the 2014 white paper asserting 'comprehensive jurisdiction,' and
 *   decisively since the 2020 national security legislation, the
 *   sovereignty-primacy understanding of One Country, Two Systems operates as
 *   the effective constitution of Hong Kong political life: local autonomy
 *   exists at the center's discretion, security categories are drawn broadly
 *   enough to reach speech, journalism, electioneering, and association,
 *   enforcement organs sit inside the territory answering to Beijing, and the
 *   opposition's organizational infrastructure has been prosecuted,
 *   dissolved, or driven into exile. The judiciary retains ordinary
 *   independence except on security dockets, where juries are dispensed with
 *   and Standing Committee interpretation binds downward. This story is one
 *   reading of a contested kernel: it instantiates
 *   sovereignty_primacy_reading only, as a clean epsilon-invariant constraint
 *   over the standing arrangement; the autonomy-primacy and
 *   balanced-coexistence siblings are separate files linked through the
 *   network section, each assessing the same arrangement by its own lights
 *   with its own epsilon and victim structure.
 *
 * KEY AGENTS:
 *   - prc_central_authority: Agenda setter (institutional/arbitrage) — issues the framework's meaning, interprets the Basic Law, deploys the security legislation; collects final authority over which local arrangements stand
 *   - mainland_security_agencies: Beneficiary (institutional/arbitrage) — gained in-territory jurisdiction, personnel, and budget through the 2020 legislation
 *   - hk_pro_beijing_establishment: Beneficiary (powerful/constrained) — holds offices, media concessions, and patronage contingent on the arrangement continuing in its current form
 *   - hk_pro_democracy_activists: Primary target (powerless/identity_locked) — prosecuted, imprisoned, or exiled; movement identity binds those who remain
 *   - hk_independent_journalists: Target (moderate/trapped) — last mass-circulation critical newsroom closed and frozen; survivors operate under red lines
 *   - hk_opposition_politicians: Target (moderate/trapped) — disqualified en masse, prosecuted for organizing primaries, parties dissolved
 *   - hk_civil_society_groups: Target (powerless/trapped) — unions, churches, and associations deregistered or emptied by leader prosecutions
 *   - hk_judiciary: Institutional target (institutional/constrained) — administers the system while losing independence on security dockets; overseas judges resigning
 *   - hk_business_community: Incidental beneficiary and cost-bearer (powerful/mobile) — collects stability and market access; absorbs compliance red lines
 *   - hk_diaspora_activists: Excluded voice (moderate/arbitrage) — advocates from abroad under extraterritorial warrants and bounties
 *   - foreign_treaty_counterparties: Excluded voice (institutional/constrained) — signatories whose guarantees were reclassified as historical documents
 *   - international_law_community: Analytical observer (analytical/analytical) — tracks the framework against treaty texts and covenants; documentation without enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__sovereignty_primacy_reading, 0.82).
domain_priors:suppression_score(one_country_two_systems_framework__sovereignty_primacy_reading, 0.88).
domain_priors:theater_ratio(one_country_two_systems_framework__sovereignty_primacy_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__sovereignty_primacy_reading, snare).
narrative_ontology:human_readable(one_country_two_systems_framework__sovereignty_primacy_reading, "One Country, Two Systems — Sovereignty Primacy Reading (Security Override Regime)").
narrative_ontology:topic_domain(one_country_two_systems_framework__sovereignty_primacy_reading, "constitutional/political").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__sovereignty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__sovereignty_primacy_reading, '4ffbee79-eef3-4bcc-b32a-b9879b98e9e7').
narrative_ontology:cs_kernel_codification('4ffbee79-eef3-4bcc-b32a-b9879b98e9e7', fixed_text).
narrative_ontology:cs_authority_grounding('4ffbee79-eef3-4bcc-b32a-b9879b98e9e7', extraction).
narrative_ontology:cs_interpretation_layer_present('4ffbee79-eef3-4bcc-b32a-b9879b98e9e7').
narrative_ontology:cs_reading_relation('4ffbee79-eef3-4bcc-b32a-b9879b98e9e7', one_country_two_systems_framework__autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('4ffbee79-eef3-4bcc-b32a-b9879b98e9e7', one_country_two_systems_framework__balanced_coexistence_reading, forecloses).
narrative_ontology:cs_axiom('4ffbee79-eef3-4bcc-b32a-b9879b98e9e7', foundational, sovereign_supremacy_over_delegated_autonomy).
narrative_ontology:cs_axiom_status(sovereign_supremacy_over_delegated_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('4ffbee79-eef3-4bcc-b32a-b9879b98e9e7', sovereign_supremacy_over_delegated_autonomy, conventional).
narrative_ontology:cs_axiom('4ffbee79-eef3-4bcc-b32a-b9879b98e9e7', foundational, national_security_override_is_unconditional).
narrative_ontology:cs_axiom_status(national_security_override_is_unconditional, holdable).
narrative_ontology:cs_axiom_grounding('4ffbee79-eef3-4bcc-b32a-b9879b98e9e7', national_security_override_is_unconditional, instrumental).
narrative_ontology:cs_axiom('4ffbee79-eef3-4bcc-b32a-b9879b98e9e7', secondary, patriots_administer_hong_kong).
narrative_ontology:cs_axiom_status(patriots_administer_hong_kong, holdable).
narrative_ontology:cs_axiom_grounding('4ffbee79-eef3-4bcc-b32a-b9879b98e9e7', patriots_administer_hong_kong, conventional).
narrative_ontology:cs_reference_frame('4ffbee79-eef3-4bcc-b32a-b9879b98e9e7', delegated_autonomy_revocable_by_center).
narrative_ontology:cs_drift_state('4ffbee79-eef3-4bcc-b32a-b9879b98e9e7', post_nsl_consolidation, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('4ffbee79-eef3-4bcc-b32a-b9879b98e9e7', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_authority).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, hk_pro_beijing_establishment).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, mainland_security_agencies).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hk_pro_democracy_activists).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hk_independent_journalists).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hk_opposition_politicians).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hk_civil_society_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, hk_judiciary).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, hk_business_community).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hk_judiciary).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hk_business_community).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__sovereignty_primacy_reading, comprehensive_jurisdiction_doctrine).
narrative_ontology:constraint_vindicates(one_country_two_systems_framework__sovereignty_primacy_reading, patriots_administering_hong_kong_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issues the authoritative statements of what the framework means, interprets the Basic Law through its Standing Committee, imposed the 2020 national security legislation, and redesigned the territory's electoral system. Final authority over which local arrangements stand rests here, and no external body reviews its decisions. It wrote the rules it applies and can revise them unilaterally.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_authority, agenda_setter,
    institutional, generational, arbitrage, global).

% Gained an in-territory office, personnel, budget, and case jurisdiction through the 2020 legislation, operating alongside and above local police. Staff answer to ministries in Beijing rather than to local courts for most conduct. The mandate expanded again with the 2024 local ordinance.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, mainland_security_agencies, beneficiary,
    institutional, generational, arbitrage, continental).

% Holds legislative, executive, and advisory seats, media concessions, and contracted opportunities that flow from alignment with the center. Its position depends on the arrangement continuing in its current form; defection would forfeit office, income, and standing. It supplies the votes and local legitimation the center's decisions require.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_pro_beijing_establishment, beneficiary,
    powerful, biographical, constrained, national).

% Organized primaries, protests, unions, and mutual-aid networks before 2020; since then dozens are imprisoned under national-security and sedition charges, others await trial in remand, and the remainder operate under surveillance or from abroad. Leaving the territory means abandoning constituents and often family; staying means legal jeopardy. Most cannot separate themselves from the cause without losing the identity the work built.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_pro_democracy_activists, payer,
    powerless, biographical, identity_locked, regional).

% Ran the territory's last mass-circulation critical newspaper until its closure and asset freeze in 2021; editors and publishers remain in custody awaiting trial. Surviving outlets publish under published and unpublished red lines, practicing escalating self-censorship. Relocation abroad ends access to sources and audience at home.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_independent_journalists, payer,
    moderate, biographical, trapped, regional).

% Were disqualified from office wholesale after the 2020 oath and candidacy-review changes; more than forty were prosecuted for organizing primary elections; established parties dissolved rather than face registration risk. Those still at liberty cannot stand for any seat that matters; those abroad retain titles without institutions.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_opposition_politicians, payer,
    moderate, biographical, trapped, regional).

% Trade-union confederations, professional associations, churches, and neighborhood groups deregistered, disbanded, or emptied by member flight after leaders faced charges. Rebuilding requires registration under ordinances that now screen for security risk, so the sector operates informally or not at all.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_civil_society_groups, payer,
    powerless, biographical, trapped, regional).

% Administers the courts that apply the security legislation: no juries on designated cases, judges selected by a committee the chief executive chairs, and Standing Committee interpretation binding on meaning. Several overseas non-permanent judges resigned citing the environment; the bench continues because declining security dockets is not available to it. Its independence now varies by docket.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_judiciary, payer,
    institutional, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__sovereignty_primacy_reading, hk_judiciary, beneficiary).

% Collects stability, market access, and financing ties that the integrated arrangement secures, and leading houses hold advisory seats. It also absorbs compliance costs: patriotism screening for boards, red lines on public statements, and periodic collateral damage when sectors fall afoul of security priorities. Capital and residency abroad give it the easiest departure of any local seat, and some have used it.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_business_community, beneficiary,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__sovereignty_primacy_reading, hk_business_community, payer).

% Continue advocacy, lobbying, and media work from London, Toronto, Taipei, and elsewhere after leaving ahead of or following prosecution. Arrest warrants and bounties attach to named individuals abroad; they hold no standing in any Hong Kong institution and cannot safely return.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hk_diaspora_activists, excluded,
    moderate, biographical, arbitrage, global).

% Signed and deposited the instruments guaranteeing the territory's way of life and now hold statements of protest rather than mechanisms: the depositary role is nominal, parliamentary scrutiny found the assurances degraded, and the central government declares the instruments historical documents. Sanctions and visa measures are the remaining levers.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, foreign_treaty_counterparties, excluded,
    institutional, generational, constrained, global).

% Academic lawyers, bar associations, and treaty bodies track the framework against the Joint Declaration, the Basic Law's own text, and the covenants extended to the territory. They publish opinions and submit reviews and have no enforcement power; their function is documentation and standard-keeping.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, international_law_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_authority).
narrative_ontology:fixing_cost_class(one_country_two_systems_framework__sovereignty_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single sovereign frame for governing Hong Kong's place in the People's Republic: unified defense and foreign policy, a formal channel (Standing Committee interpretation) for resolving Basic Law disputes, and a defined path for integrating the territory's institutions with central priorities.
% TRANSFER_FUNCTION: Moves final decision-making authority over Hong Kong's political arrangements, the exercise space for political speech and assembly, and judicial independence on security dockets from Hong Kong residents and courts to the central authorities and their security organs; moves offices, honors, and contracted opportunities to the pro-Beijing establishment.
% ABSENT_VOICES: Imprisoned and exiled opposition figures, the shuttered newsroom's staff, deregistered unions and churches, and the Joint Declaration counterparties would all object if present. They are outside the room because the enforcement machinery removed them (custody, exile, dissolution) or because the central government reclassified the treaty framework as concluded; the diaspora speaks from abroad under warrant, and the counterparties speak through statements no longer attached to any mechanism.
% DISAPPEARANCE_RATIONALE: If the override machinery vanished overnight, prosecuted opposition figures would appeal through restored channels, the closed newsroom would reopen, deregistered societies would re-form, and the electoral system would revert to contested competition; the central authorities would lose the instruments by which local arrangements are currently finalized, and the treaty counterparties would regain live claims.
% FOUNDING_PROBLEM: Managing the 1997 resumption of sovereignty over a capitalist territory with a common-law system and a population holding mass-emigration options, without collapsing its economic function or breaching the assurances given to Britain and to Hong Kong residents.
% FOUNDING_PROBLEM_CORROBORATION: The Joint Declaration texts and the 1980s diplomatic and parliamentary records attest the founding problem from outside the benefiting parties. On current status: central white papers and officials attest a live security problem requiring the override; United Nations human-rights reviews, foreign-government statements, and disbarred or exiled jurists attest that the security frame now operates over a dismantled opposition. Both sides corroborate from outside the beneficiary set; the dispute itself is the finding.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__sovereignty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__sovereignty_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__sovereignty_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(one_country_two_systems_framework__sovereignty_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__sovereignty_primacy_reading, 0.82, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__sovereignty_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(one_country_two_systems_framework__sovereignty_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(one_country_two_systems_framework__sovereignty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.82) because the arrangement's operative output since 2020 is the removal of political self-determination: speech and assembly categories criminalized, the opposition's organizational infrastructure dissolved, and the electorate's choice set narrowed to vetted candidates. Suppression is authored higher still (0.88) and is a raw structural property — unlike extractiveness, it is not scaled by power or scope anywhere in the engine's computation. Persistence rests on continuously operated enforcement (a dedicated national security police unit, an in-territory mainland office, security-docket trials without juries) rather than on participant preference, hence requires_active_enforcement. Theater_ratio 0.38 reflects heavy real enforcement volume beneath a growing ceremonial layer (loyalty oaths, patriotic curricula, anniversary mobilizations) that performs the arrangement rather than operating it. Accessibility_collapse 0.62: internal political alternatives have collapsed almost completely, but the exit door (emigration) remains traversable at cost, so alternatives are narrowed rather than annihilated. Resistance 0.58: mass mobilization ended after 2020, but exile advocacy, underground mutual aid, electoral abstention, and foreign-government measures continue. The measurement series runs on one shared seven-point grid (2014, 2016, 2019, 2020, 2021, 2023, 2025) so every tracked metric is authored at every examined time point; the trajectories are monotonic ratchets, not cycles — each step (white paper, interpretation, legislation, electoral overhaul, local ordinance) locked in the previous one. Coalition potential among the powerless victims existed in 2019-2020 and was specifically dismantled (primary-organizer prosecutions), which is why the powerless seats are modeled as unable to convert numbers into leverage.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently from the same structure. The center's seat combines agenda-setting with arbitrage-grade exit (it wrote and can rewrite the rules), so it experiences near-zero effective burden and reads the arrangement as restored proper order. The trapped and identity-locked payer seats experience the maximum effective burden the structure can deliver: their exits are foreclosed precisely by the enforcement machinery. The business community's mobility dampens its experienced burden toward symmetry — stability collected against compliance paid. The judiciary splits by docket: full independence retained on commercial matters, none on security matters, so its experienced burden is partial and docket-dependent. Two institutional-power seats diverge sharply — the security agencies collect jurisdiction while the judiciary surrenders it — showing that at equal nominal power, role and exit, not rank, determine what the arrangement costs each seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: the center (arbitrage exit) sits nearest the beneficiary pole; the establishment (constrained) and the security agencies (arbitrage) also derive subsidy — office, jurisdiction, budget. Victim declarations drive high directionality, amplified by exit foreclosure: activists (identity_locked), journalists, politicians, and civil society groups (all trapped) sit near the full-target pole, and the engine amplifies their effective extraction accordingly. The judiciary is a payer with institutional power and constrained exit — high d, partially damped by its retained commercial-docket independence. The business community is a declared beneficiary with mobile exit, placing it near-symmetric; its secondary payer role is why it is not modeled as a pure beneficiary. The diaspora and foreign counterparties are excluded voices — outside the conversation, not inside the transfer loop. Gain_flow names the center because final authority and the rents of control demonstrably accrue there; agency budgets and establishment patronage are downstream distributions of that accrual.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — managing the 1997 resumption of sovereignty without collapsing the territory's economic function — was completed in 1997 and stayed completed. The arrangement persisted and was repurposed: the 2014 white paper and the 2020 legislation converted a transition framework into a permanent override mechanism serving a newly asserted security mandate. The R5 interview records status 'contested' rather than 'dead' because the benefiting parties assert a live successor problem; corroboration from outside the beneficiary set (UN reviews, foreign governments, exiled jurists) attests that the security frame operates over a dismantled opposition. The classification disciplines two errors: it prevents the genuine coordination layer (one sovereign frame for defense, foreign policy, and Basic Law adjudication) from laundering the override machinery as pure coordination, and it prevents the real enforcement volume from being misread as inertial theater — the arrests are functional for the arrangement's purpose. With status contested and verdict world_rearranges, the mismatch consumer flags the genealogy dispute for investigation rather than issuing an automatic zombie verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading (sovereignty_primacy_reading) of the one_country_two_systems_framework kernel; what would the sibling readings change structurally?',
    'Comparative compilation of the sibling files (autonomy_primacy_reading, balanced_coexistence_reading): identical referent arrangement, reading-indexed epsilon and victim sets; the disagreement locates in whether Basic Law autonomy is revocable delegation, enforceable treaty guarantee, or negotiable boundary.',
    'Under autonomy_primacy_reading the same arrangement computes with victims defined by breached treaty guarantees and enforceability claims against the center; under balanced_coexistence_reading it computes as a failed-accommodation hybrid. Seat classifications shift accordingly; this file''s snare claim holds within this reading''s frame, and the divergence across readings is itself the measurement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: one of three rival readings of the One Country, Two Systems kernel.').

omega_variable(
    security_threat_proportionality,
    'Is the national-security threat profile (secession, subversion, terrorism, foreign collusion) empirically proportional to the override machinery deployed, or does the security frame function over a politically defined opposition?',
    'Prosecution-composition analysis (violent-offense share of national-security cases), declassified threat assessments, and comparative counter-terrorism benchmarks from jurisdictions that do not prosecute opposition organization as such.',
    'If the threat is materially pretextual, the coordination cover collapses and the snare classification firms; if a genuine threat exists at deployed scale, part of the measured suppression is ordinary security governance and the arrangement shades toward a coordination-plus-extraction hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(security_threat_proportionality, empirical, 'Whether the security override tracks a real threat or covers political control.').

omega_variable(
    delegation_vs_subordination_boundary,
    'Can ''delegated autonomy'' remain meaningfully distinct from subordination when revocation is unilateral, interpretation is monopolized internally, and no external review channel survives?',
    'Conceptual analysis against comparative devolution and federal cases: identify any preserved limit that actually binds the center; if none exists, delegation collapses into subordination and the reading''s own description becomes the arrangement''s admission.',
    'If delegation collapses, the original Basic Law layer loses coordination standing retroactively and the whole framework reads as control architecture; if a binding limit is identified, part of the current burden is breach of design rather than design itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(delegation_vs_subordination_boundary, conceptual, 'Whether revocable delegation differs structurally from subordination.').

omega_variable(
    diaspora_exit_interpretation,
    'Does the post-2020 emigration wave represent voluntary exit the arrangement permits, or coerced flight that extends its pressure beyond the territory?',
    'Emigrant-panel surveys on stated motives, timing correlation with enforcement events, and destination-country asylum recognition rates for Hong Kong applicants.',
    'If exit is coerced, effective suppression exceeds the in-territory measure and accessibility_collapse is understated; if voluntary, the arrangement retains a consent channel that moderates its classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diaspora_exit_interpretation, empirical, 'Voluntary-exit versus coerced-flight interpretation of the emigration wave.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__sovereignty_primacy_reading, 2014, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ocs_sovereignty_primacy_tr_t2014, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2014, 0.15).
narrative_ontology:measurement_basis(ocs_sovereignty_primacy_tr_t2014, observed).
narrative_ontology:measurement(ocs_sovereignty_primacy_tr_t2016, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2016, 0.18).
narrative_ontology:measurement_basis(ocs_sovereignty_primacy_tr_t2016, observed).
narrative_ontology:measurement(ocs_sovereignty_primacy_tr_t2019, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2019, 0.2).
narrative_ontology:measurement_basis(ocs_sovereignty_primacy_tr_t2019, observed).
narrative_ontology:measurement(ocs_sovereignty_primacy_tr_t2020, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2020, 0.28).
narrative_ontology:measurement_basis(ocs_sovereignty_primacy_tr_t2020, observed).
narrative_ontology:measurement(ocs_sovereignty_primacy_tr_t2021, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2021, 0.33).
narrative_ontology:measurement_basis(ocs_sovereignty_primacy_tr_t2021, observed).
narrative_ontology:measurement(ocs_sovereignty_primacy_tr_t2023, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2023, 0.36).
narrative_ontology:measurement_basis(ocs_sovereignty_primacy_tr_t2023, observed).
narrative_ontology:measurement(ocs_sovereignty_primacy_tr_t2025, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 2025, 0.38).
narrative_ontology:measurement_basis(ocs_sovereignty_primacy_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(ocs_sovereignty_primacy_be_t2014, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2014, 0.42).
narrative_ontology:measurement_basis(ocs_sovereignty_primacy_be_t2014, observed).
narrative_ontology:measurement(ocs_sovereignty_primacy_be_t2016, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2016, 0.5).
narrative_ontology:measurement_basis(ocs_sovereignty_primacy_be_t2016, observed).
narrative_ontology:measurement(ocs_sovereignty_primacy_be_t2019, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2019, 0.58).
narrative_ontology:measurement_basis(ocs_sovereignty_primacy_be_t2019, observed).
narrative_ontology:measurement(ocs_sovereignty_primacy_be_t2020, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2020, 0.72).
narrative_ontology:measurement_basis(ocs_sovereignty_primacy_be_t2020, observed).
narrative_ontology:measurement(ocs_sovereignty_primacy_be_t2021, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2021, 0.78).
narrative_ontology:measurement_basis(ocs_sovereignty_primacy_be_t2021, observed).
narrative_ontology:measurement(ocs_sovereignty_primacy_be_t2023, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2023, 0.8).
narrative_ontology:measurement_basis(ocs_sovereignty_primacy_be_t2023, observed).
narrative_ontology:measurement(ocs_sovereignty_primacy_be_t2025, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 2025, 0.82).
narrative_ontology:measurement_basis(ocs_sovereignty_primacy_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(ocs_sovereignty_primacy_su_t2014, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2014, 0.35).
narrative_ontology:measurement_basis(ocs_sovereignty_primacy_su_t2014, observed).
narrative_ontology:measurement(ocs_sovereignty_primacy_su_t2016, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2016, 0.4).
narrative_ontology:measurement_basis(ocs_sovereignty_primacy_su_t2016, observed).
narrative_ontology:measurement(ocs_sovereignty_primacy_su_t2019, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2019, 0.55).
narrative_ontology:measurement_basis(ocs_sovereignty_primacy_su_t2019, observed).
narrative_ontology:measurement(ocs_sovereignty_primacy_su_t2020, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2020, 0.75).
narrative_ontology:measurement_basis(ocs_sovereignty_primacy_su_t2020, observed).
narrative_ontology:measurement(ocs_sovereignty_primacy_su_t2021, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2021, 0.83).
narrative_ontology:measurement_basis(ocs_sovereignty_primacy_su_t2021, observed).
narrative_ontology:measurement(ocs_sovereignty_primacy_su_t2023, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2023, 0.86).
narrative_ontology:measurement_basis(ocs_sovereignty_primacy_su_t2023, observed).
narrative_ontology:measurement(ocs_sovereignty_primacy_su_t2025, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 2025, 0.88).
narrative_ontology:measurement_basis(ocs_sovereignty_primacy_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__sovereignty_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, autonomy_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, balanced_coexistence_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'One Country, Two Systems' decomposes into three structurally distinct constraints — one per rival reading of the kernel. This file instantiates sovereignty_primacy_reading; autonomy_primacy_reading and balanced_coexistence_reading are separate files with their own epsilon, beneficiary/victim structures, and classifications. The family linkage runs downstream from this reading: its enforcement creates the conditions (dismantled opposition, captured interpretation, reclassified treaties) under which the sibling readings survive only as external commitments, while the siblings' persistence as live positions elsewhere is what keeps this reading's claims contested rather than settled.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
