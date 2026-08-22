% ============================================================================
% CONSTRAINT STORY: speech_protection_kernel__democratic_participation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_speech_protection_kernel__democratic_participation_reading, []).

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
 *   constraint_id: speech_protection_kernel__democratic_participation_reading
 *   human_readable: Democratic Self-Governance Speech Protection Hierarchy
 *   domain: constitutional/political/communication
 *
 * SUMMARY:
 *   This constraint story captures the democratic participation reading of
 *   the speech protection kernel — the view that the First Amendment's core
 *   purpose is protecting political expression necessary for self-governance,
 *   creating an internal hierarchy where political speech receives the
 *   highest protection while commercial, obscene, hate-adjacent, and false
 *   speech are more readily restricted. The reading originated in Footnote 4
 *   of United States v. Carolene Products (1938) and was systematized by Ely
 *   and others as a process-theoretic justification for judicial review. The
 *   constraint operates as a tangled rope: it genuinely coordinates
 *   democratic self-governance (beneficiaries: citizen_electorate,
 *   political_organizers, opposition_parties, investigative_journalists,
 *   whistleblowers) while asymmetrically extracting from lower-tier speakers
 *   (victims: commercial_speakers, hate_speech_proponents,
 *   obscenity_distributors, false_advertising_actors) through active judicial
 *   enforcement of the hierarchy.
 *
 * KEY AGENTS:
 *   - supreme_court: Agenda setter (institutional/analytical) — authors and enforces the hierarchy
 *   - citizen_electorate: Primary beneficiary (organized/constrained) — depends on political speech protection for self-governance
 *   - political_organizers: Beneficiary (moderate/constrained) — core activity is the constraint's primary coordination target
 *   - opposition_parties: Beneficiary (powerful/constrained) — rely on hierarchy to challenge incumbents
 *   - investigative_journalists: Beneficiary (moderate/constrained) — publish protected political speech
 *   - whistleblowers: Beneficiary (powerless/trapped) — theoretically protected but practically vulnerable
 *   - commercial_speakers: Payer (powerful/constrained) — bear intermediate scrutiny the hierarchy imposes
 *   - hate_speech_proponents: Payer (moderate/constrained) — face greater restriction at hierarchy's boundary
 *   - obscenity_distributors: Payer (moderate/constrained) — categorically excluded from protection
 *   - false_advertising_actors: Payer (moderate/constrained) — least protected, freely regulable
 *   - legal_academy: Observer (institutional/analytical) — structures interpretive environment
 *   - international_human_rights_bodies: Observer (institutional/analytical) — external legitimacy pressure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(speech_protection_kernel__democratic_participation_reading, 0.18).
domain_priors:suppression_score(speech_protection_kernel__democratic_participation_reading, 0.22).
domain_priors:theater_ratio(speech_protection_kernel__democratic_participation_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(speech_protection_kernel__democratic_participation_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(speech_protection_kernel__democratic_participation_reading, tangled_rope).
narrative_ontology:human_readable(speech_protection_kernel__democratic_participation_reading, "Democratic Self-Governance Speech Protection Hierarchy").
narrative_ontology:topic_domain(speech_protection_kernel__democratic_participation_reading, "constitutional/political/communication").

domain_priors:requires_active_enforcement(speech_protection_kernel__democratic_participation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(speech_protection_kernel__democratic_participation_reading, '00af441c-7bd0-4b04-9b63-11bbb1f58884').
narrative_ontology:cs_kernel_codification('00af441c-7bd0-4b04-9b63-11bbb1f58884', formalized).
narrative_ontology:cs_authority_grounding('00af441c-7bd0-4b04-9b63-11bbb1f58884', lineage).
narrative_ontology:cs_interpretation_layer_present('00af441c-7bd0-4b04-9b63-11bbb1f58884').
narrative_ontology:cs_reading_relation('00af441c-7bd0-4b04-9b63-11bbb1f58884', speech_protection_kernel__absolutist_reading, coexists_with).
narrative_ontology:cs_reading_relation('00af441c-7bd0-4b04-9b63-11bbb1f58884', speech_protection_kernel__harm_threshold_reading, coexists_with).
narrative_ontology:cs_reading_relation('00af441c-7bd0-4b04-9b63-11bbb1f58884', speech_protection_kernel__marketplace_reading, influences).
narrative_ontology:cs_reading_relation('00af441c-7bd0-4b04-9b63-11bbb1f58884', speech_protection_kernel__dignity_reading, coexists_with).
narrative_ontology:cs_axiom('00af441c-7bd0-4b04-9b63-11bbb1f58884', foundational, political_speech_apex_protection).
narrative_ontology:cs_axiom_status(political_speech_apex_protection, holdable).
narrative_ontology:cs_axiom_grounding('00af441c-7bd0-4b04-9b63-11bbb1f58884', political_speech_apex_protection, deontological).
narrative_ontology:cs_axiom('00af441c-7bd0-4b04-9b63-11bbb1f58884', foundational, democratic_process_justifies_hierarchy).
narrative_ontology:cs_axiom_status(democratic_process_justifies_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('00af441c-7bd0-4b04-9b63-11bbb1f58884', democratic_process_justifies_hierarchy, instrumental).
narrative_ontology:cs_reference_frame('00af441c-7bd0-4b04-9b63-11bbb1f58884', carolene_products_footnote_four_framework).
narrative_ontology:cs_drift_state('00af441c-7bd0-4b04-9b63-11bbb1f58884', contemporary_citizens_united_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('00af441c-7bd0-4b04-9b63-11bbb1f58884', '').
narrative_ontology:cs_kernel_id(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, citizen_electorate).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, political_organizers).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, opposition_parties).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, investigative_journalists).
narrative_ontology:constraint_beneficiary(speech_protection_kernel__democratic_participation_reading, whistleblowers).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, commercial_speakers).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, hate_speech_proponents).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, obscenity_distributors).
narrative_ontology:constraint_victim(speech_protection_kernel__democratic_participation_reading, false_advertising_actors).
narrative_ontology:constraint_vindicates(speech_protection_kernel__democratic_participation_reading, democratic_self_governance_requires_uninhibited_political_discourse).
narrative_ontology:constraint_vindicates(speech_protection_kernel__democratic_participation_reading, political_speech_is_first_among_equals_in_first_amendment_hierarchy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authoritatively adjudicates the boundaries of protected speech through constitutional interpretation. Sets the hierarchy that privileges political speech. Its doctrine determines which restrictions survive scrutiny and which categories of speech fall outside core protection.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, supreme_court, agenda_setter,
    institutional, generational, analytical, national).

% Depends on robust political discourse to exercise self-governance. The hierarchy's protection of political speech directly enables their participation. Exit from the constraint is practically unavailable — they cannot opt out of the constitutional framework that structures their political speech rights.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, citizen_electorate, beneficiary,
    organized, biographical, constrained, national).

% Build movements, campaigns, and opposition through political speech that receives the highest protection. Their core activity is the constraint's primary beneficiary. They remain subject to time-place-manner restrictions and cannot escape the regulatory framework governing political activity.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, political_organizers, beneficiary,
    moderate, biographical, constrained, national).

% Rely on the hierarchy's protection to challenge incumbents without state suppression. The constraint's structure is their primary shield against retaliation. They operate within the same constitutional framework and cannot exit its jurisdiction.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, opposition_parties, beneficiary,
    powerful, biographical, constrained, national).

% Publish political speech that exposes government misconduct, receiving near-absolute protection under the hierarchy. Their work depends on the constraint's core protection. They face legal risks when publishing classified information but cannot exit the constitutional regime.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, investigative_journalists, beneficiary,
    moderate, biographical, constrained, national).

% Disclose government wrongdoing as political speech essential to self-governance. The hierarchy theoretically protects them, but practical retaliation (prosecution, career destruction) makes their position precarious. They are structurally trapped — no exit from state power, and the constraint's protection is often aspirational for them.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, whistleblowers, beneficiary,
    powerless, immediate, trapped, national).

% Operate in a lower tier of protection (Central Hudson intermediate scrutiny). Their speech is regulable in ways political speech is not. They bear the cost of the hierarchy — restrictions that would be unconstitutional if applied to political speech are permitted for commercial speech. They cannot exit the regulatory framework but can lobby and litigate within it.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, commercial_speakers, payer,
    powerful, biographical, constrained, national).

% Advocate views that fall outside the hierarchy's core protection. While not categorically unprotected in US doctrine, their speech faces greater restriction and social sanction. They bear the cost of the hierarchy's line-drawing. Exit from the constitutional framework is unavailable; they contest boundaries within it.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, hate_speech_proponents, payer,
    moderate, biographical, constrained, national).

% Operate in a category the Court has placed outside First Amendment protection entirely (Miller test). They bear the full cost of the hierarchy's exclusionary boundary. No exit from the categorical exclusion; their speech is structurally suppressible.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, obscenity_distributors, payer,
    moderate, biographical, constrained, national).

% Engage in commercial speech that is demonstrably false — the least protected category. They bear the hierarchy's cost most directly: their speech is freely regulable. Exit from the regulatory regime is impossible; they face enforcement within it.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, false_advertising_actors, payer,
    moderate, immediate, constrained, national).

% Produces the doctrinal frameworks, critiques, and historical analyses that shape how the hierarchy is understood and applied. Does not directly collect or pay under the constraint but structures the interpretive environment in which the agenda-setter operates.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, legal_academy, observer,
    institutional, generational, analytical, national).

% Monitor and critique the hierarchy from comparative and treaty-law perspectives (ICCPR Article 19, regional courts). Their assessments create external legitimacy pressure but carry no binding enforcement within the US constitutional system.
narrative_ontology:constraint_stakeholder(speech_protection_kernel__democratic_participation_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of democratic self-governance: citizens cannot govern themselves without a protected space for political discourse, debate, and opposition. The hierarchy coordinates by designating political speech as the coordination-critical tier that must remain maximally open.
% TRANSFER_FUNCTION: Transfers protection and immunity from restriction from non-political speech categories (commercial, obscene, hate-adjacent, false) to political speech. The state's regulatory power is concentrated on lower-tier speech; political speakers receive a subsidy of near-absolute protection paid for by the greater suppressibility of other speech.
% ABSENT_VOICES: Minority-language communities whose political speech receives formal protection but lacks practical access; incarcerated persons whose political speech is heavily restricted; non-citizens excluded from the electoral self-governance the hierarchy serves; future generations who inherit the doctrine but had no voice in its formation.
% DISAPPEARANCE_RATIONALE: If the hierarchy vanished overnight, political speech would lose its privileged status. Commercial speech regulations would face strict scrutiny; hate speech and obscenity restrictions would require the same compelling-interest showing as political speech restrictions. The entire architecture of First Amendment law would collapse into either near-absolutism (if all speech gets political-tier protection) or a flattened intermediate scrutiny (if all speech gets commercial-tier review). The regulatory landscape for elections, campaign finance, protest, and press freedom would fundamentally reorganize.
% FOUNDING_PROBLEM: The Lochner-era Court's use of substantive due process to strike down economic regulation created a legitimacy crisis. The democratic participation reading emerged (Carolene Products Footnote 4, 1938) to redirect judicial scrutiny toward protecting the political process itself — ensuring that the channels of democratic change remain open — rather than protecting economic liberty.
% FOUNDING_PROBLEM_CORROBORATION: Ely's 'Democracy and Distrust' (1980) provides the canonical scholarly corroboration from outside the beneficiary set — a structural argument for process-based review that does not simply serve the Court's institutional interest. Critics (Bork, Scalia, contemporary originalists) contest that the founding problem was ever the Court's to solve, arguing the hierarchy is an illegitimate judicial invention. The corroboration is thus genuinely contested, not self-asserted.
narrative_ontology:disappearance_verdict(speech_protection_kernel__democratic_participation_reading, world_rearranges).
narrative_ontology:founding_problem_status(speech_protection_kernel__democratic_participation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(speech_protection_kernel__democratic_participation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(speech_protection_kernel__democratic_participation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(speech_protection_kernel__democratic_participation_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(speech_protection_kernel__democratic_participation_reading_tests).
:- end_tests(speech_protection_kernel__democratic_participation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed type (tangled_rope) reflects the dual structure: genuine coordination of democratic self-governance (without protected political speech, the collective action problem of democracy cannot be solved) combined with asymmetric extraction (non-political speakers bear regulatory burdens that would be unconstitutional if applied to political speech). Extractiveness (0.18) is modest because the hierarchy's primary operation is protective — it subsidizes political speech by concentrating regulatory power elsewhere. Suppression (0.22) reflects the active enforcement required to maintain the hierarchy's boundaries (categorical exclusions, tiered scrutiny). Theater ratio (0.15) is low because the doctrine's operational core (political speech protection) is genuinely functional, not performative. Accessibility collapse (0.72) is high because the hierarchy's categorical structure (political vs. commercial vs. obscene) makes alternative regulatory architectures difficult to articulate within the framework. Resistance (0.35) is moderate — commercial speakers and civil libertarians contest boundaries but the hierarchy's core has been stable since the 1960s.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (commercial speakers, hate speech proponents, obscenity distributors) and beneficiary seats (citizen electorate, political organizers) experience fundamentally different constraints. From the beneficiary perspective, the hierarchy is a protective mountain — political speech is simply *protected*, the hierarchy is the natural architecture of democratic self-governance. From the payer perspective, the same structure is an enforced extraction mechanism — their speech is suppressible *because* the hierarchy exists to privilege political speech. The Court (agenda_setter) experiences the constraint as its own doctrinal creation — a coordination tool it built and maintains. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The Supreme Court (agenda_setter, institutional, analytical exit) sits at the beneficiary end of directionality — it administers the constraint and its institutional legitimacy is tied to the hierarchy's coherence. Citizen electorate and political organizers (beneficiaries, organized/moderate, constrained exit) are structural beneficiaries with low directionality — the constraint subsidizes their core activity. Opposition parties (beneficiary, powerful, constrained) similarly benefit. Investigative journalists and whistleblowers are beneficiaries but with higher effective directionality: journalists (moderate, constrained) have institutional protections; whistleblowers (powerless, trapped) are structurally vulnerable despite theoretical protection. Commercial speakers (payer, powerful, constrained) sit at the target end — they bear the hierarchy's regulatory concentration. Hate speech proponents, obscenity distributors, and false advertising actors (payers, moderate, constrained) face progressively higher directionality as their speech categories receive less protection. Observers (legal academy, international bodies) sit at the analytical end with near-zero directionality — they neither collect nor pay.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Lochner-era legitimacy crisis, need to protect democratic process channels) was live in 1938. By the 1970s, the process-protection rationale had become the dominant justification for judicial review. Today, the status is contested: originalists argue the hierarchy has no textual basis and the founding problem was never the Court's to solve; living constitutionalists argue the problem remains live because democratic capture and gerrymandering still threaten the political process. The hierarchy persists partly because it solves a genuine coordination problem (democratic discourse needs protection) and partly because the Court's institutional authority now depends on it — a classic mandatrophy signature where the original mandate has been succeeded by institutional self-justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_kernel_reading,
    'How does the democratic participation reading''s structural delta (internal hierarchy privileging political speech) relate to the sibling readings of the speech protection kernel?',
    'Comparative structural analysis of each reading''s beneficiary/victim sets, extractiveness profiles, and coordination functions. The kernel frame requires each reading to be ε-invariant — this reading''s ε (0.18) is assessed against the standing arrangement (current First Amendment doctrine) from this reading''s lights, not against the reading''s endorsed alternative.',
    'If the democratic participation reading''s hierarchy is structurally distinct from sibling readings (different beneficiary/victim sets, different ε), they are separate constraints linked by network.affects_constraints, not variants of one constraint. The ε-invariance principle requires this decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_kernel_reading, conceptual, 'Kernel/reading decomposition: this constraint is one reading of a contested kernel; sibling readings instantiate different constraints with different ε values.').

omega_variable(
    hierarchy_boundary_stability,
    'Is the political/non-political speech boundary structurally stable, or does it collapse under doctrinal pressure (e.g., Citizens United treating corporate political spending as core political speech, commercial speech doctrine creeping toward strict scrutiny)?',
    'Track doctrinal trajectory: if the boundary erodes (commercial speech gets higher protection, political speech definition expands to include corporate spending), the hierarchy''s coordination function degrades and extractiveness shifts.',
    'Boundary collapse would increase extractiveness (more speech claims political-tier protection) and raise suppression (Court must work harder to maintain distinctions). Could shift classification toward snare if hierarchy becomes pure judicial preference without coordination anchor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hierarchy_boundary_stability, empirical, 'Whether the internal hierarchy''s categorical boundaries are stable or eroding under doctrinal pressure.').

omega_variable(
    whistleblower_protection_gap,
    'Does the hierarchy''s theoretical protection for whistleblowers as political speakers have operational force, or is the powerless/trapped position of whistleblowers a structural feature revealing the hierarchy''s extractive core?',
    'Empirical study of whistleblower outcomes: prosecution rates, Espionage Act usage, practical protection vs. theoretical doctrine.',
    'If whistleblowers are systematically unprotected despite the hierarchy''s logic, the coordination function is compromised and the constraint''s extractiveness is higher than measured — the hierarchy protects *some* political speakers (organized, powerful) while sacrificing others (powerless, trapped).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(whistleblower_protection_gap, empirical, 'Whether the hierarchy''s protection extends to its most vulnerable theoretical beneficiaries or fails them structurally.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(speech_protection_kernel__democratic_participation_reading, 1937, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(speech_protection_democratic_tr_t1937, speech_protection_kernel__democratic_participation_reading, theater_ratio, 1937, 0.42).
narrative_ontology:measurement(speech_protection_democratic_tr_t1964, speech_protection_kernel__democratic_participation_reading, theater_ratio, 1964, 0.25).
narrative_ontology:measurement(speech_protection_democratic_tr_t1971, speech_protection_kernel__democratic_participation_reading, theater_ratio, 1971, 0.18).
narrative_ontology:measurement(speech_protection_democratic_tr_t1989, speech_protection_kernel__democratic_participation_reading, theater_ratio, 1989, 0.12).
narrative_ontology:measurement(speech_protection_democratic_tr_t2010, speech_protection_kernel__democratic_participation_reading, theater_ratio, 2010, 0.14).
narrative_ontology:measurement(speech_protection_democratic_tr_t2026, speech_protection_kernel__democratic_participation_reading, theater_ratio, 2026, 0.15).

% Extraction over time
narrative_ontology:measurement(speech_protection_democratic_be_t1937, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 1937, 0.35).
narrative_ontology:measurement(speech_protection_democratic_be_t1964, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 1964, 0.22).
narrative_ontology:measurement(speech_protection_democratic_be_t1971, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 1971, 0.18).
narrative_ontology:measurement(speech_protection_democratic_be_t1989, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 1989, 0.15).
narrative_ontology:measurement(speech_protection_democratic_be_t2010, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 2010, 0.16).
narrative_ontology:measurement(speech_protection_democratic_be_t2026, speech_protection_kernel__democratic_participation_reading, base_extractiveness, 2026, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(speech_protection_democratic_su_t1937, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 1937, 0.65).
narrative_ontology:measurement(speech_protection_democratic_su_t1964, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 1964, 0.38).
narrative_ontology:measurement(speech_protection_democratic_su_t1971, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 1971, 0.28).
narrative_ontology:measurement(speech_protection_democratic_su_t1989, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 1989, 0.22).
narrative_ontology:measurement(speech_protection_democratic_su_t2010, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 2010, 0.2).
narrative_ontology:measurement(speech_protection_democratic_su_t2026, speech_protection_kernel__democratic_participation_reading, suppression_requirement, 2026, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(speech_protection_kernel__democratic_participation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(speech_protection_kernel__democratic_participation_reading, 0.08).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__absolutist_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__harm_threshold_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__marketplace_reading).
narrative_ontology:affects_constraint(speech_protection_kernel__democratic_participation_reading, speech_protection_kernel__dignity_reading).

% DUAL FORMULATION NOTE:
% This constraint is the democratic_participation_reading of the speech_protection_kernel. It differs from sibling readings by positing an internal hierarchy (political speech > other speech) justified by democratic self-governance. The absolutist_reading rejects hierarchy; the harm_threshold_reading conditions protection on harm absence; the marketplace_reading posits truth-discovery as the coordination function; the dignity_reading conditions protection on non-subordination. All five readings are structurally distinct constraints with different beneficiary/victim sets and ε values, linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(speech_protection_kernel__democratic_participation_reading, powerless, 0.85).
constraint_indexing:directionality_override(speech_protection_kernel__democratic_participation_reading, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
