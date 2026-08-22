% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__incidental_collection_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fisa_702_statutory_text__incidental_collection_reading, []).

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
 *   constraint_id: fisa_702_statutory_text__incidental_collection_reading
 *   human_readable: FISA 702 Incidental Collection Reading — Retention and Warrantless Query of U.S. Person Communications
 *   domain: constitutional/legal/national-security
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the fisa_702_statutory_text
 *   kernel: the incidental_collection_reading, under which the statute
 *   permits retaining and querying — without individualized warrants —
 *   communications of U.S. persons acquired incidentally to targeting of
 *   non-U.S. persons abroad, when justified by a foreign-intelligence
 *   purpose. The epsilon referent is the standing arrangement this reading
 *   governs — the retention-and-query practice as it actually operates —
 *   assessed by the reading's own lights: the reading concedes that U.S.
 *   persons bear a real privacy cost and holds it justified by
 *   foreign-intelligence necessity, which prices the extraction near
 *   mid-scale (0.45) rather than at zero. The sibling readings are separate
 *   constraints, not parts of this one: the foreign_target_strict_reading
 *   (incidental U.S. person data minimized and inaccessible for domestic
 *   purposes) and the constitutional_floor_reading (Fourth Amendment warrant
 *   required for 702 queries regardless of statutory text) each instantiate a
 *   different arrangement with a different victim set and a different
 *   epsilon. The authority structure grounding this reading is
 *   extraction-shaped: the implementing institutions draw substantial benefit
 *   from kernel stability, and the interpretive layer (agency-authored
 *   targeting and minimization procedures under FISC approval) absorbs
 *   operational drift without surfacing statutory revision. The claim/metric
 *   gap is deliberate: the constraint is CLAIMED as tangled_rope — a genuine,
 *   still-live foreign-intelligence coordination function carrying asymmetric
 *   extraction of U.S.-person privacy through the same structure — while the
 *   metrics describe what the practice does; the engine measures the
 *   divergence per seat.
 *
 * KEY AGENTS:
 *   - signals_intelligence_agencies: agenda-setter and primary beneficiary (institutional/arbitrage) — authors the procedures, runs the database, collects the analytical product
 *   - fbi_domestic_investigative_units: beneficiary with query access (institutional/constrained) — warrantless queries for domestic-adjacent investigations, remediated after documented violations
 *   - foreign_intelligence_surveillance_court: procedural gatekeeper (institutional/constrained) — approves and polices the framework, hears the government's case
 *   - us_person_communicants: primary target (powerless/trapped) — communications swept in incidentally, no notice, no exit
 *   - sensitive_occupation_us_persons: concentrated targets (moderate/constrained) — journalists, lawyers, legislators disproportionately present and queried
 *   - congressional_reauthorizers: statutory agenda-setters (institutional/constrained) — renew or amend on a multi-year cycle
 *   - civil_liberties_litigators: excluded challengers (moderate/constrained) — standing-barred from merits review
 *   - privacy_oversight_board: analytical observer (moderate/analytical) — audits and reports without enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__incidental_collection_reading, 0.45).
domain_priors:suppression_score(fisa_702_statutory_text__incidental_collection_reading, 0.65).
domain_priors:theater_ratio(fisa_702_statutory_text__incidental_collection_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__incidental_collection_reading, tangled_rope).
narrative_ontology:human_readable(fisa_702_statutory_text__incidental_collection_reading, "FISA 702 Incidental Collection Reading — Retention and Warrantless Query of U.S. Person Communications").
narrative_ontology:topic_domain(fisa_702_statutory_text__incidental_collection_reading, "constitutional/legal/national-security").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__incidental_collection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__incidental_collection_reading, '2f6af814-fd95-4061-8ab0-e6b931320e30').
narrative_ontology:cs_kernel_codification('2f6af814-fd95-4061-8ab0-e6b931320e30', fixed_text).
narrative_ontology:cs_authority_grounding('2f6af814-fd95-4061-8ab0-e6b931320e30', extraction).
narrative_ontology:cs_interpretation_layer_present('2f6af814-fd95-4061-8ab0-e6b931320e30').
narrative_ontology:cs_reading_relation('2f6af814-fd95-4061-8ab0-e6b931320e30', fisa_702_statutory_text__foreign_target_strict_reading, forecloses).
narrative_ontology:cs_reading_relation('2f6af814-fd95-4061-8ab0-e6b931320e30', fisa_702_statutory_text__constitutional_floor_reading, forecloses).
narrative_ontology:cs_axiom('2f6af814-fd95-4061-8ab0-e6b931320e30', foundational, foreign_intelligence_purpose_justifies_warrantless_us_person_query).
narrative_ontology:cs_axiom_status(foreign_intelligence_purpose_justifies_warrantless_us_person_query, holdable).
narrative_ontology:cs_axiom_grounding('2f6af814-fd95-4061-8ab0-e6b931320e30', foreign_intelligence_purpose_justifies_warrantless_us_person_query, instrumental).
narrative_ontology:cs_axiom('2f6af814-fd95-4061-8ab0-e6b931320e30', foundational, administrative_minimization_substitutes_for_individualized_warrant).
narrative_ontology:cs_axiom_status(administrative_minimization_substitutes_for_individualized_warrant, holdable).
narrative_ontology:cs_axiom_grounding('2f6af814-fd95-4061-8ab0-e6b931320e30', administrative_minimization_substitutes_for_individualized_warrant, conventional).
narrative_ontology:cs_reference_frame('2f6af814-fd95-4061-8ab0-e6b931320e30', statutory_foreign_intelligence_primacy).
narrative_ontology:cs_drift_state('2f6af814-fd95-4061-8ab0-e6b931320e30', contemporary_post_reform_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('2f6af814-fd95-4061-8ab0-e6b931320e30', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, signals_intelligence_agencies).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, fbi_domestic_investigative_units).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, us_person_communicants).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, sensitive_occupation_us_persons).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__incidental_collection_reading, executive_foreign_intelligence_primacy).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__incidental_collection_reading, administrative_minimization_sufficiency).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Author the targeting and minimization procedures that operationalize the reading and run the collection infrastructure that fills the 702 database. Communications of U.S. persons acquired incidentally to foreign targeting flow into their holdings and remain retrievable for foreign-intelligence queries. Their exit is wide: they can shift collection between authorities, adjust procedures at each reauthorization, and re-task collection to compensate for any single limit.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, signals_intelligence_agencies, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(fisa_702_statutory_text__incidental_collection_reading, signals_intelligence_agencies, beneficiary).

% Query the 702 database in counterintelligence, cyber, and criminal investigations without obtaining individualized warrants. Documented improper queries of U.S. persons — including members of Congress, journalists, and a state judge — led to court-imposed remediation and a statutory warrant requirement for one narrow query category in 2024. Their access depends on the reading remaining operative; losing it would mean warrant process for each query of a U.S.-person identifier.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, fbi_domestic_investigative_units, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(fisa_702_statutory_text__incidental_collection_reading, fbi_domestic_investigative_units, agenda_setter).

% Approves the targeting and minimization procedures through which the reading operates, hears the government's applications with amicus participation since 2015, and polices compliance through occasional remedial orders. It cannot rewrite the statutory text and its docket is one-sided by design; its role is bounded by the framework it administers.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, foreign_intelligence_surveillance_court, agenda_setter,
    institutional, generational, constrained, national).

% Anyone in the United States whose communications touch a targeted non-U.S. person abroad — family, commerce, scholarship, journalism, diplomacy — has those communications swept into the database. They receive no notice, cannot learn whether their communications were retained or queried, and cannot opt out of communicating with people overseas. Their only exit is not communicating.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, us_person_communicants, payer,
    powerless, biographical, trapped, national).

% Journalists, lawyers, clergy, and legislators whose work involves regular contact with people abroad are disproportionately present in the database. Attorney-client communications and press-source confidentiality ride on the same wires; documented queries have targeted members of Congress and reporters. They can change professions or route communications through other means at real cost to their work, but cannot seal the channel itself.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, sensitive_occupation_us_persons, payer,
    moderate, biographical, constrained, national).

% Set the statutory frame and must renew it on a multi-year cycle; each cycle surfaces reform proposals ranging from query warrant requirements to full expiration. Members' own communications have appeared in improper queries, yet the coalition that renews the arrangement has held across four cycles. Their exit runs through the amendment process, which the intelligence committees' jurisdiction makes slow.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, congressional_reauthorizers, agenda_setter,
    institutional, biographical, constrained, national).

% Bring challenges to the query practice and advocate the alternative readings in public and academic fora. Federal courts have dismissed their suits for lack of standing — no plaintiff can show which of their communications were taken — so they have never reached a merits hearing on the query question. They are inside the political conversation but outside the courtroom that would adjudicate the reading.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, civil_liberties_litigators, excluded,
    moderate, biographical, constrained, national).

% Audits the program, quantifies query volumes and compliance failures, and reports to Congress and the public. Its recommendations carry no enforcement power; it documented both the intelligence value and the compliance problems, then watched subsequent reauthorizations proceed without adopting most of its proposals.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, privacy_oversight_board, observer,
    moderate, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fisa_702_statutory_text__incidental_collection_reading, signals_intelligence_agencies).
narrative_ontology:fixing_cost_class(fisa_702_statutory_text__incidental_collection_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a statutory authorization and oversight wrapper for collecting foreign intelligence at scale: agencies target non-U.S. persons abroad under court-approved procedures instead of seeking a warrant per target, and a single legal framework coordinates collection, retention, and interagency access that would otherwise be ad hoc executive action.
% TRANSFER_FUNCTION: Moves the content and metadata of U.S. persons' communications — acquired incidentally to foreign targeting — from private channels into government databases, and moves query access to that data from a warrant-gated process to an agency-administered one; the decision right over Americans' private communications transfers from individualized judicial process to executive procedure.
% ABSENT_VOICES: The queried U.S. persons themselves: no notice, no standing — Amnesty v. Clapper (2013) and Wikimedia v. NSA (2021) dismissed challengers before merits review, so the people whose communications are at stake have never been in the courtroom. Civil-liberties litigators and the affected communicants are structurally absent from the court process, which hears the government with amicus participation only.
% DISAPPEARANCE_RATIONALE: If the reading vanished overnight — incidental data minimized to irretrievability and queries requiring individualized warrants — the intelligence community would lose its principal repository for foreign-intelligence analysis touching U.S. persons, the FBI would need warrant process for database queries, collection incentives would shift toward avoiding U.S.-person touchpoints, and the oversight apparatus built around the practice would reorganize around whatever replaced it.
% FOUNDING_PROBLEM: The 1978 warrant-per-target FISA model could not scale to foreign-intelligence work against diffuse networks — terrorism, proliferation, cyber intrusion — where targets are numerous, foreign, and fast-moving, and where the government cannot show probable cause for each interception in advance. The 2008 amendments were built to authorize large-scale foreign collection under a legal framework rather than leave it to unreviewed executive action.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the benefiting parties: the 9/11 Commission record and the 2008 legislative history attest the scale problem the arrangement was built to solve; the Privacy and Civil Liberties Oversight Board's 2014 report — from a body that simultaneously documented the U.S.-person costs — affirmed the program's foreign-intelligence value; declassified court opinions acknowledge the intelligence necessity. No corroborating source outside the beneficiary set claims the founding problem is dead; the contest is over the cost side, not the problem's existence.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__incidental_collection_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__incidental_collection_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__incidental_collection_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fisa_702_statutory_text__incidental_collection_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__incidental_collection_reading, 0.45, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fisa_702_statutory_text__incidental_collection_reading_tests).
:- end_tests(fisa_702_statutory_text__incidental_collection_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.45: the reading's own frame concedes the privacy cost (retention plus warrantless query of U.S.-person content at scale) while holding it justified — a mid-scale, not maximal, extraction from the reading's own lights. Suppression (0.65) is a raw structural property, unscaled by power or scope: the warrant alternative for these queries is foreclosed by statutory text, FISC procedure, and standing doctrine — the suppression is structural legal closure, not internalized belief. Theater (0.40) reflects an oversight layer that does real work (FISC remedial orders, PCLOB audits) while the core query practice proceeds under broad exceptions; minimization is partly functional, partly label. Accessibility collapse (0.55): the alternatives do not vanish once the arrangement is understood — the strict and constitutional-floor readings remain live legal positions — but they are operationally foreclosed, which places collapse mid-scale rather than high. Resistance (0.60): sustained litigation, four contested reauthorization cycles, and recurring reform proposals that have narrowed but not displaced the practice. The measurement grid is shared across all three tracked metrics (t = 0, 3, 6, 9, 12, 15, 17 over a 2008-2025 span). Base extractiveness rises through the query-expansion and about-collection years, peaks after the 2013 disclosures made the practice fully visible (the rise reflects both real practice expansion and post-disclosure measurement visibility), then partially retreats under the USA FREEDOM Act and the 2024 reforms — a reauthorization-cycle oscillation in which each reform wave narrows the margins while the core practice is renewed; the cycle is driven by the statutory sunset-and-renew rhythm, not by intermittent reinforcement. Suppression requirement climbs across the interval because the reading needed little defense at enactment and increasingly active defense — FISC remediation, compliance apparatus, reauthorization coalitions — as litigation and disclosure pressure mounted.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats compute differently from the same structure. From the agencies' seat the arrangement is a lawfully authorized, FISC-approved foreign-intelligence framework whose U.S.-person handling is governed by procedures they wrote and a court approved; from the trapped communicants' seat the same arrangement is a warrantless search of their private communications that they cannot see, contest, or exit. The FISC seat sits between: it polices a framework it did not design and cannot rewrite. The engine derives these per-seat types from power, exit, and declared position; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary declarations (signals_intelligence_agencies, fbi_domestic_investigative_units) place those seats near the subsidized end: the arrangement delivers collection and query access to them and their exit is wide or workable. The victim declarations (us_person_communicants, sensitive_occupation_us_persons) place those seats near the full-target end, amplified by trapped and constrained exit — a U.S. person cannot arbitrage away incidental collection, and the sensitive-occupation group bears concentrated query incidence without an exit that preserves their work. The institutional agenda-setters (FISC, congressional reauthorizers) derive mid-low directionality: they administer and renew a structure from which they draw no direct analytical product. No directionality overrides are used: the beneficiary/victim declarations plus exit options produce the structural relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — foreign-intelligence collection that cannot scale under warrant-per-target process — is still live, corroborated by oversight bodies that simultaneously document the cost side, so this is not a mandate outliving its function; the contest is over the distribution of cost, not obsolescence. The tangled_rope claim is what prevents misclassification in both directions: reading the arrangement as pure coordination would erase the asymmetric extraction the compliance record documents (improper queries of legislators and journalists, standing-barred challenge, retention under broad exceptions); reading it as pure extraction would erase the genuine, corroborated foreign-intelligence function the arrangement performs. The R5 mismatch check is clean: founding_problem_status=live with disappearance_verdict=world_rearranges — arrangements demonstrably depend on it and the founding problem persists, so no zombie flag is expected.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reading_contest_location,
    'This story instantiates the incidental_collection_reading of the fisa_702_statutory_text kernel; the disagreement among readings is located in the operative rule for incidentally collected U.S. person communications — which sibling reading''s rule governs if the contest resolves, and how do the victim set and epsilon move with it?',
    'Merits adjudication of a 702 query challenge (requiring a standing-satisfying plaintiff) or statutory amendment redefining minimization; either would install one sibling''s operative rule and transfer the victim set.',
    'Under foreign_target_strict_reading the victim set contracts to the acquisition stage and epsilon falls toward coordination cost; under constitutional_floor_reading every queried U.S. person becomes a warrantless-search claimant and epsilon rises; under this reading the victim set is defined by query incidence, as authored.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_contest_location, conceptual, 'Committer structure: one of three readings of the 702 text; the structural delta between readings is the operative rule for incidentally collected U.S. person data.').

omega_variable(
    standing_barrier_persistence,
    'Can any U.S. person obtain merits review of the warrantless query practice, or does standing doctrine permanently insulate the reading from judicial correction?',
    'A challenge brought by a plaintiff with documented query incidence — the FBI''s disclosed improper queries of identifiable individuals supply candidate plaintiffs — reaching a merits decision.',
    'If standing stays closed, the reading''s persistence rests entirely on political reauthorization cycles and the suppression measure is fully structural; if opened, judicial displacement becomes available and the constitutional_floor_reading could be executed by a court.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(standing_barrier_persistence, empirical, 'Whether standing doctrine permanently bars merits review of the reading.').

omega_variable(
    minimization_function_vs_cover,
    'Do minimization procedures functionally constrain retention and querying, or do their broad exceptions make the protective label largely performative?',
    'Declassified FISC compliance reviews and PCLOB audits quantifying query volumes against exception usage and actual retention practice.',
    'If exceptions swallow the rule, the effective closure of the privacy alternative is higher than the authored suppression and the reading drifts toward pure extraction; if the procedures genuinely bind, part of the authored extraction is coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minimization_function_vs_cover, empirical, 'Whether the minimization layer functions or covers.').

omega_variable(
    warrant_requirement_intelligence_cost,
    'How much foreign-intelligence value would a warrant requirement for U.S.-person queries actually cost — is the implementing agencies'' prohibitive-cost claim empirically accurate?',
    'Post-hoc assessment of intelligence loss following the 2024 warrant mandate for FBI evidence-of-crime queries, and PCLOB-style quantification of query yield against case outcomes.',
    'A small realized cost would collapse the instrumental axiom''s empirical premise and make fixing cheap, shifting the reading toward pure extraction; a large realized cost would vindicate the coordination share and the tangled_rope claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(warrant_requirement_intelligence_cost, empirical, 'The contested empirical premise of the reading''s instrumental axiom.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__incidental_collection_reading, 0, 17).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(f702_incidental_tr_t0, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement_basis(f702_incidental_tr_t0, observed).
narrative_ontology:measurement(f702_incidental_tr_t3, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 3, 0.25).
narrative_ontology:measurement_basis(f702_incidental_tr_t3, observed).
narrative_ontology:measurement(f702_incidental_tr_t6, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 6, 0.32).
narrative_ontology:measurement_basis(f702_incidental_tr_t6, observed).
narrative_ontology:measurement(f702_incidental_tr_t9, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 9, 0.35).
narrative_ontology:measurement_basis(f702_incidental_tr_t9, observed).
narrative_ontology:measurement(f702_incidental_tr_t12, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 12, 0.38).
narrative_ontology:measurement_basis(f702_incidental_tr_t12, observed).
narrative_ontology:measurement(f702_incidental_tr_t15, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement_basis(f702_incidental_tr_t15, observed).
narrative_ontology:measurement(f702_incidental_tr_t17, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 17, 0.4).
narrative_ontology:measurement_basis(f702_incidental_tr_t17, observed).

% Extraction over time
narrative_ontology:measurement(f702_incidental_be_t0, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(f702_incidental_be_t0, observed).
narrative_ontology:measurement(f702_incidental_be_t3, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 3, 0.38).
narrative_ontology:measurement_basis(f702_incidental_be_t3, observed).
narrative_ontology:measurement(f702_incidental_be_t6, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 6, 0.5).
narrative_ontology:measurement_basis(f702_incidental_be_t6, observed).
narrative_ontology:measurement(f702_incidental_be_t9, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 9, 0.52).
narrative_ontology:measurement_basis(f702_incidental_be_t9, observed).
narrative_ontology:measurement(f702_incidental_be_t12, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement_basis(f702_incidental_be_t12, observed).
narrative_ontology:measurement(f702_incidental_be_t15, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 15, 0.48).
narrative_ontology:measurement_basis(f702_incidental_be_t15, observed).
narrative_ontology:measurement(f702_incidental_be_t17, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 17, 0.45).
narrative_ontology:measurement_basis(f702_incidental_be_t17, observed).

% Suppression requirement over time
narrative_ontology:measurement(f702_incidental_su_t0, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(f702_incidental_su_t0, observed).
narrative_ontology:measurement(f702_incidental_su_t3, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 3, 0.45).
narrative_ontology:measurement_basis(f702_incidental_su_t3, observed).
narrative_ontology:measurement(f702_incidental_su_t6, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 6, 0.6).
narrative_ontology:measurement_basis(f702_incidental_su_t6, observed).
narrative_ontology:measurement(f702_incidental_su_t9, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 9, 0.63).
narrative_ontology:measurement_basis(f702_incidental_su_t9, observed).
narrative_ontology:measurement(f702_incidental_su_t12, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 12, 0.65).
narrative_ontology:measurement_basis(f702_incidental_su_t12, observed).
narrative_ontology:measurement(f702_incidental_su_t15, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(f702_incidental_su_t15, observed).
narrative_ontology:measurement(f702_incidental_su_t17, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 17, 0.65).
narrative_ontology:measurement_basis(f702_incidental_su_t17, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__incidental_collection_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text__foreign_target_strict_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text__constitutional_floor_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one statutory kernel (fisa_702_statutory_text) decomposes into three structurally distinct constraints because the readings assign different operative rules — and therefore different epsilon values and victim sets — to the same text. The incidental_collection_reading (this file, epsilon≈0.45, victims defined by query incidence) sits downstream of the collection premise the foreign_target_strict_reading would narrow (incidental data minimized and sealed, victims at acquisition) and upstream of the constitutional_floor_reading's displacement claim (warrant required regardless of text, victims = all queried persons). Each story is epsilon-invariant on its own reading; the family links make the contest navigable without folding it into any single story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
