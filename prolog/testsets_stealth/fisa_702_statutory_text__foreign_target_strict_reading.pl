% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__foreign_target_strict_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fisa_702_statutory_text__foreign_target_strict_reading, []).

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
 *   constraint_id: fisa_702_statutory_text__foreign_target_strict_reading
 *   human_readable: FISA Section 702 — Foreign-Target Strict Reading (Minimized Incidental Collection)
 *   domain: constitutional/legal/national_security
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel. The kernel is
 *   the enacted statutory text governing foreign-intelligence collection;
 *   this file authors the foreign_target_strict_reading only: collection is
 *   confined to communications where the sender and the primary investigative
 *   interest are non-U.S. persons abroad, and incidentally collected
 *   U.S.-person data must be minimized to genuine inaccessibility for
 *   domestic purposes — deletion or sealing, not mere access control — with
 *   any subsequent use requiring an individualized warrant. Sibling readings
 *   (incidental_collection_reading, constitutional_floor_reading) are
 *   separate constraints in separate files; nothing about the contest is
 *   averaged into this story's metrics. The claim/metric gap is deliberate
 *   and load-bearing: the claimed type (tangled_rope) is my independent
 *   structural verdict that the arrangement carries a genuine coordination
 *   function AND asymmetric extraction borne by a non-consenting foreign
 *   class, while the authored extractiveness (0.15) is the reading-indexed
 *   value over the reading's own lights — rights-holders retain
 *   constitutional protections under this regime. The engine computes
 *   per-seat classifications from the structural data; where the
 *   foreign-target seat computes far more extractively than the
 *   reading-indexed scalar, that divergence is the measurement the corpus
 *   exists to take. KEY AGENTS (by structural relationship): -
 *   non_us_persons_abroad: primary target (powerless/trapped) — bears the
 *   program's principal burden; no notice, no process, no forum -
 *   us_persons_incidentally_collected: protected beneficiary
 *   (powerless/trapped) — shielded by the minimization duty they cannot opt
 *   into or out of - intelligence_collection_agencies: operator-payer with
 *   agenda-setting reach (institutional/arbitrage) — pays in compliance and
 *   capability, receives the program's product, retains substitute
 *   authorities - fbi_domestic_crime_investigators: constrained payer
 *   (institutional/constrained) — same nominal power as the collectors,
 *   categorically closed corpus - fisc: agenda-setter
 *   (institutional/analytical) — approves, conditions, or refuses;
 *   adjudicative seat - congressional_oversight_committees: institutional
 *   beneficiary (institutional/constrained) — gains an auditable standard,
 *   limited verification between reauthorizations -
 *   privacy_advocacy_organizations: organized beneficiary-observer
 *   (organized/mobile) — mission advanced, monitors deviations
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__foreign_target_strict_reading, 0.15).
domain_priors:suppression_score(fisa_702_statutory_text__foreign_target_strict_reading, 0.45).
domain_priors:theater_ratio(fisa_702_statutory_text__foreign_target_strict_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(fisa_702_statutory_text__foreign_target_strict_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__foreign_target_strict_reading, tangled_rope).
narrative_ontology:human_readable(fisa_702_statutory_text__foreign_target_strict_reading, "FISA Section 702 — Foreign-Target Strict Reading (Minimized Incidental Collection)").
narrative_ontology:topic_domain(fisa_702_statutory_text__foreign_target_strict_reading, "constitutional/legal/national_security").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__foreign_target_strict_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__foreign_target_strict_reading, '7cf54ae2-4f14-4212-9974-c3dcc2df97f2').
narrative_ontology:cs_kernel_codification('7cf54ae2-4f14-4212-9974-c3dcc2df97f2', fixed_text).
narrative_ontology:cs_authority_grounding('7cf54ae2-4f14-4212-9974-c3dcc2df97f2', distributed).
narrative_ontology:cs_reading_relation('7cf54ae2-4f14-4212-9974-c3dcc2df97f2', fisa_702_statutory_text__incidental_collection_reading, forecloses).
narrative_ontology:cs_reading_relation('7cf54ae2-4f14-4212-9974-c3dcc2df97f2', fisa_702_statutory_text__constitutional_floor_reading, coexists_with).
narrative_ontology:cs_axiom('7cf54ae2-4f14-4212-9974-c3dcc2df97f2', foundational, foreign_target_language_exhaustive_of_collection_authority).
narrative_ontology:cs_axiom_status(foreign_target_language_exhaustive_of_collection_authority, holdable).
narrative_ontology:cs_axiom_grounding('7cf54ae2-4f14-4212-9974-c3dcc2df97f2', foreign_target_language_exhaustive_of_collection_authority, conventional).
narrative_ontology:cs_axiom('7cf54ae2-4f14-4212-9974-c3dcc2df97f2', secondary, minimization_requires_inaccessibility_not_access_control).
narrative_ontology:cs_axiom_status(minimization_requires_inaccessibility_not_access_control, holdable).
narrative_ontology:cs_axiom_grounding('7cf54ae2-4f14-4212-9974-c3dcc2df97f2', minimization_requires_inaccessibility_not_access_control, conventional).
narrative_ontology:cs_reference_frame('7cf54ae2-4f14-4212-9974-c3dcc2df97f2', enacted_foreign_target_bargain).
narrative_ontology:cs_drift_state('7cf54ae2-4f14-4212-9974-c3dcc2df97f2', contemporary_reauthorization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7cf54ae2-4f14-4212-9974-c3dcc2df97f2', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, us_persons_incidentally_collected).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, congressional_oversight_committees).
narrative_ontology:constraint_victim(fisa_702_statutory_text__foreign_target_strict_reading, non_us_persons_abroad).
narrative_ontology:constraint_victim(fisa_702_statutory_text__foreign_target_strict_reading, intelligence_collection_agencies).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__foreign_target_strict_reading, privacy_advocacy_organizations).
narrative_ontology:constraint_victim(fisa_702_statutory_text__foreign_target_strict_reading, fbi_domestic_crime_investigators).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__foreign_target_strict_reading, statutory_textualism_in_surveillance_law).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__foreign_target_strict_reading, minimization_as_deletion_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Communicate across borders for work, family, and travel; a portion of those communications enters U.S. collection streams aimed at overseas targets. Under this reading, once their identifiers or content surface in the corpus, minimization procedures must purge or seal the material: it may not be queried for domestic cases, shared with domestic law enforcement, or retained beyond foreign-intelligence need, and any future use requires an individualized judicial warrant. They cannot opt out of the collection stream and typically learn of exposure only through breach disclosures or criminal discovery.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, us_persons_incidentally_collected, beneficiary,
    powerless, biographical, trapped, national).

% Receive classified reports, hold hearings, and vote on periodic reauthorization. The bright-line foreign-target rule gives them an auditable standard to demand compliance against; they can amend the statute or condition reauthorization on tighter safeguards, but classified program detail and intelligence-community resistance limit how far they can verify practice between reauthorization cycles.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, congressional_oversight_committees, beneficiary,
    institutional, generational, constrained, national).

% Operate the collection: task selectors against overseas targets, ingest the resulting traffic, and run the foreign-intelligence analysis the program exists to supply. They draft the minimization procedures the court approves, bear the compliance burden of deleting or sealing U.S.-person material, and lose the option of searching the corpus for domestic leads. They retain substitutes — individually warranted orders under other statutory titles, collection under executive-order authority conducted wholly overseas — and they set much of the program's day-to-day shape subject to court sign-off.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, intelligence_collection_agencies, payer,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(fisa_702_statutory_text__foreign_target_strict_reading, intelligence_collection_agencies, agenda_setter).

% Work criminal cases inside the United States. Under this reading the corpus is closed to them: no browsing for leads on U.S.-person identifiers without a probable-cause order, however useful the database would be. Their substitute path runs through the ordinary warrant process, which is slower and requires articulable suspicion before the search rather than after.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, fbi_domestic_crime_investigators, payer,
    institutional, biographical, constrained, national).

% Reviews targeting and minimization procedures, approves certifications, adjudicates compliance disputes, and can order destruction of improperly held material. It neither collects nor consumes intelligence; its position is adjudicative, and its practical move in a dispute is limited to approving, conditioning, or refusing the government's proposed procedures.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, fisc, agenda_setter,
    institutional, generational, analytical, national).

% Litigate standing-limited challenges, publish technical and legal analyses of compliance records, and campaign at each reauthorization. Enactment of their core demand — deletion rather than access-control for incidentally collected U.S.-person data — advances their institutional mission; they also monitor and publicly report deviations from it.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, privacy_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(fisa_702_statutory_text__foreign_target_strict_reading, privacy_advocacy_organizations, observer).

% Live and communicate outside the United States. Their international communications are the program's intended subject matter: selected, ingested, retained, and analyzed without notice, without individualized process, and without recourse in U.S. courts under the rights tradition this reading inherits. Switching providers, jurisdictions, or languages short of going silent does not reliably remove them from collection against overseas targets.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__foreign_target_strict_reading, non_us_persons_abroad, payer,
    powerless, biographical, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fisa_702_statutory_text__foreign_target_strict_reading, intelligence_collection_agencies).
narrative_ontology:fixing_cost_class(fisa_702_statutory_text__foreign_target_strict_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the post-Church-Committee collective-action problem of executive surveillance: a bright statutory line confining collection to non-U.S.-person targets abroad lets agencies gather foreign intelligence at scale while giving domestic rights-holders an enforceable boundary and overseers an auditable rule.
% TRANSFER_FUNCTION: Moves communicative privacy: the content and metadata of non-U.S. persons' international communications flow from those communicants into government databases held by the intelligence agencies for foreign-intelligence use; incidentally collected U.S.-person material, under this reading, is transferred out of usable circulation entirely — deleted or sealed pending an individualized warrant.
% ABSENT_VOICES: Non-U.S. persons abroad — the population whose communications constitute the program — hold no seat anywhere in the structure: no notice, no appearance in court proceedings, no standing in U.S. courts under the governing standing doctrine. Criminal defendants whose cases touch derivatively derived evidence also learn of collection origins late and incompletely. Their objections enter only as filtered through advocacy organizations and amicus filings.
% DISAPPEARANCE_RATIONALE: If the foreign-target boundary and its minimization duties vanished overnight, the corpus would open to domestic querying, incidentally collected U.S.-person content would flow into domestic case files, the court's role would shrink toward routine approval, and the statutory bargain would collapse into either unrestricted query practice or immediate constitutional litigation over warrantless searches — the surveillance economy reorganizes around whichever reading fills the vacuum.
% FOUNDING_PROBLEM: The Church Committee findings revealed decades of warrantless domestic surveillance conducted under claims of foreign-intelligence necessity. The statutory scheme, and this reading of it, was built to solve a single problem: how a democracy conducts necessary foreign-intelligence surveillance without recreating unchecked spying on its own residents.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: the Church Committee's published record establishes the founding problem; successive Privacy and Civil Liberties Oversight Board reports, Department of Justice Inspector General audits, and the surveillance court's own published opinions documenting recurring query noncompliance attest that the problem remains recurrently live rather than solved.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__foreign_target_strict_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__foreign_target_strict_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__foreign_target_strict_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fisa_702_statutory_text__foreign_target_strict_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__foreign_target_strict_reading, 0.15, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fisa_702_statutory_text__foreign_target_strict_reading_tests).
:- end_tests(fisa_702_statutory_text__foreign_target_strict_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.15 is reading-indexed per the kernel-reading referent rule: the standing arrangement under contest is the strict-reading regime itself, assessed by this reading's own lights — under it, rights-holders keep Fourth Amendment protection, domestic querying is barred, and minimization deletes rather than gates. The declining series (0.34 to 0.15) tracks the regime converging on its own ideal: early-interval operation ran weak minimization and broad internal access; post-disclosure reforms (statutory query restrictions, mandatory audit logging, court-appointed advocates, documented destruction orders) tightened actual practice toward the text this reading defends. Suppression 0.45 is a raw, unscaled structural figure: the coercive machinery aims almost entirely at the agencies (court orders, compliance audits, destruction mandates, program-termination exposure) while the arrangement denies the surveilled class any alternative channel; rights-holders face none of it. Theater 0.12: minimization and oversight are substantively functional after the reform period, with residual ritual in recurring compliance reporting. Accessibility_collapse 0.35: alternatives persist on every side — substitute legal authorities for collectors, the ordinary warrant path for domestic investigators, encryption and routing for communicants — so understanding the constraint does not annihilate exits. Resistance 0.5: the intelligence community and its allies actively resist tightening at each reauthorization while the reform coalition presses the opposite way; the constraint is defended and attacked in the open. All three tracked series run on one shared seven-point grid (every metric authored at every examined time point); the suppression_requirement series is authored because the story specifically traces enforcement-capacity build-up and hardening, not merely extraction drift.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the U.S.-person seat this is a protective boundary with near-zero personal cost; from the foreign-communicant seat the same structure is total exposure — powerless, trapped, no forum — computing at the extreme target end despite the reading's low indexed scalar. Among same-level institutional actors, the collector seat and the domestic-investigator seat hold equal nominal power yet diverge sharply: collectors keep arbitrage-grade substitutes (other titles, overseas executive-order authority) while domestic investigators face a categorically closed door with only the slower warrant path — constraint-specific exit differentiation, not global standing, drives the difference. The court seat adjudicates without collecting or consuming; the committee seat benefits from legibility it cannot fully verify between reauthorization cycles. Coalition note: the foreign class's impotence is structural, not incidental — no notice and no standing foreclose even the coalition route that powerless domestic classes sometimes find.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the protected class (subsidized by the boundary) and the committees (gaining an auditable standard). Victim declarations drive high directionality for the foreign-communicant class (powerless plus trapped lands near the full-target end) and for the agencies. The agencies are the one seat where the automatic derivation would mislead: declared victim, but genuinely dual-positioned — they pay compliance and capability costs while receiving the program's entire product and holding arbitrage-grade substitutes, placing them nearer symmetric than the bare victim declaration implies. No directionality_overrides entry is authored: the schema keys overrides by power atom, and four of this story's seven seats share the institutional atom, so a single override would misapply across seats with opposed structural positions; the nuance is recorded here and in the stakeholder situations instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live, so no mandatrophy resolution is declared. The classification discipline cuts both ways here. Reading the low reading-indexed extractiveness as a pure-coordination verdict would erase the foreign class's payment — the arrangement coordinates domestic rights protection partly by leaving an unrepresented class to fund it. Reading the foreign class's exposure as pure extraction would erase the genuine coordination achievement: an enforceable domestic boundary, an auditable rule, and a functioning deletion duty that did not exist before. The tangled_rope claim keeps both facts visible and forces the enforcement requirement to be taken seriously. The mandatrophy risk sits in a specific decay path: if minimization quietly degrades from deletion into access-control theater, the regime keeps strict-reading signage while operating the incidental reading's substance — the theater_ratio series is the tripwire for exactly that substitution, and the minimization_deletion_fidelity omega routes the empirical question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (foreign_target_strict_reading) of the kernel fisa_702_statutory_text; which reading governs determines the victim set, the query rules, and the meaning of minimization — what structurally changes if a sibling reading is adopted instead?',
    'Authoritative adoption of one reading: a published appellate or surveillance-court opinion construing the text, or Congress encoding one reading in amended text at reauthorization.',
    'Under incidental_collection_reading, U.S. persons enter the victim set, warrantless query is authorized, and extraction rises sharply; under constitutional_floor_reading, the warrant requirement attaches independently of statutory construction and this reading''s textual argument becomes redundant but compatible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: which reading of the 702 text governs the arrangement.').

omega_variable(
    reading_indexed_epsilon_scope,
    'The reading-indexed extractiveness (0.15) counts extraction from rights-holders only; non-U.S. persons abroad bear the program''s principal burden with no remedy in the governing rights tradition — is the constraint''s operative extraction the reading-indexed value or the all-seats structural value?',
    'Cross-seat comparison from the stakeholder directionalities (already computable from the authored surface), plus any doctrinal extension of Fourth Amendment protection to non-U.S. persons abroad.',
    'If extraterritorial rights attach, extraction reindexes upward toward the structural value and the constraint migrates toward the snare side; if the rights tradition holds, the reading-indexed value stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_indexed_epsilon_scope, conceptual, 'Whether the reading''s low extraction figure survives a change in whose rights count.').

omega_variable(
    minimization_deletion_fidelity,
    'Does minimization-as-deletion actually render incidentally collected U.S.-person data inaccessible — faithful technical deletion, no residual copies, no backdoor querying — or does material persist in accessible form?',
    'Oversight-board technical audits, published court compliance opinions, and inspector-general forensic sampling of retained holdings and query logs.',
    'Demonstrated persistence collapses this reading''s low-extraction claim and merges its de facto regime with the incidental reading''s, dating a type transition in the temporal series.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minimization_deletion_fidelity, empirical, 'Technical fidelity of deletion-based minimization versus access-restriction practice.').

omega_variable(
    mixed_use_query_boundary,
    'Where is the line between a foreign-intelligence query and a domestic-crime use when an investigation mixes both purposes — does any domestic-purpose touch of the corpus violate this reading categorically, or only inquiries with a purely domestic predicate?',
    'Published court opinions construing query terms and purpose clauses, plus audit statistics on how query purposes are coded in practice.',
    'A porous mixed-use boundary converts the categorical prohibition into a case-by-case one, raising effective extraction from U.S. persons above the reading-indexed value without any formal change in the announced reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mixed_use_query_boundary, empirical, 'Operational boundary between foreign-intelligence and domestic-crime database use.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__foreign_target_strict_reading, 0, 18).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(f702_strict_tr_t0, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 0, 0.26).
narrative_ontology:measurement_basis(f702_strict_tr_t0, observed).
narrative_ontology:measurement(f702_strict_tr_t3, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 3, 0.23).
narrative_ontology:measurement_basis(f702_strict_tr_t3, observed).
narrative_ontology:measurement(f702_strict_tr_t6, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 6, 0.19).
narrative_ontology:measurement_basis(f702_strict_tr_t6, observed).
narrative_ontology:measurement(f702_strict_tr_t9, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 9, 0.15).
narrative_ontology:measurement_basis(f702_strict_tr_t9, observed).
narrative_ontology:measurement(f702_strict_tr_t12, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement_basis(f702_strict_tr_t12, observed).
narrative_ontology:measurement(f702_strict_tr_t15, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 15, 0.12).
narrative_ontology:measurement_basis(f702_strict_tr_t15, observed).
narrative_ontology:measurement(f702_strict_tr_t18, fisa_702_statutory_text__foreign_target_strict_reading, theater_ratio, 18, 0.12).
narrative_ontology:measurement_basis(f702_strict_tr_t18, observed).

% Extraction over time
narrative_ontology:measurement(f702_strict_be_t0, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(f702_strict_be_t0, observed).
narrative_ontology:measurement(f702_strict_be_t3, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 3, 0.3).
narrative_ontology:measurement_basis(f702_strict_be_t3, observed).
narrative_ontology:measurement(f702_strict_be_t6, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 6, 0.26).
narrative_ontology:measurement_basis(f702_strict_be_t6, observed).
narrative_ontology:measurement(f702_strict_be_t9, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 9, 0.21).
narrative_ontology:measurement_basis(f702_strict_be_t9, observed).
narrative_ontology:measurement(f702_strict_be_t12, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 12, 0.17).
narrative_ontology:measurement_basis(f702_strict_be_t12, observed).
narrative_ontology:measurement(f702_strict_be_t15, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 15, 0.16).
narrative_ontology:measurement_basis(f702_strict_be_t15, observed).
narrative_ontology:measurement(f702_strict_be_t18, fisa_702_statutory_text__foreign_target_strict_reading, base_extractiveness, 18, 0.15).
narrative_ontology:measurement_basis(f702_strict_be_t18, observed).

% Suppression requirement over time
narrative_ontology:measurement(f702_strict_su_t0, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(f702_strict_su_t0, observed).
narrative_ontology:measurement(f702_strict_su_t3, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 3, 0.27).
narrative_ontology:measurement_basis(f702_strict_su_t3, observed).
narrative_ontology:measurement(f702_strict_su_t6, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 6, 0.33).
narrative_ontology:measurement_basis(f702_strict_su_t6, observed).
narrative_ontology:measurement(f702_strict_su_t9, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 9, 0.4).
narrative_ontology:measurement_basis(f702_strict_su_t9, observed).
narrative_ontology:measurement(f702_strict_su_t12, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement_basis(f702_strict_su_t12, observed).
narrative_ontology:measurement(f702_strict_su_t15, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 15, 0.45).
narrative_ontology:measurement_basis(f702_strict_su_t15, observed).
narrative_ontology:measurement(f702_strict_su_t18, fisa_702_statutory_text__foreign_target_strict_reading, suppression_requirement, 18, 0.45).
narrative_ontology:measurement_basis(f702_strict_su_t18, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__foreign_target_strict_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text__incidental_collection_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__foreign_target_strict_reading, fisa_702_statutory_text__constitutional_floor_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'Section 702' covers at least three structurally distinct constraints — one per reading of the same statutory text. Per the epsilon-invariance principle, each reading is authored as its own story with its own extractiveness, victim set, and classification; no single story averages across readings. The statutory text is the upstream artifact all three readings cite as authority; the readings form a constraint family linked through affects_constraints. This file authors the strict reading only: U.S. persons sit outside the victim set absent an individualized warrant, minimization means deletion, and the reading-indexed extractiveness is low. The incidental_collection_reading sibling authors a materially higher epsilon over the same referent with U.S. persons inside the victim set; the constitutional_floor_reading sibling authors the warrant requirement as reading-independent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
