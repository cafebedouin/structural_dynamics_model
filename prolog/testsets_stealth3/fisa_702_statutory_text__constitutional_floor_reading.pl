% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__constitutional_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fisa_702_statutory_text__constitutional_floor_reading, []).

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
 *   constraint_id: fisa_702_statutory_text__constitutional_floor_reading
 *   human_readable: Constitutional Floor Reading: Pre-Query Warrant Requirement for 702 Content Queries
 *   domain: legal/constitutional/national_security
 *
 * SUMMARY:
 *   This story instantiates one reading of the contested FISA Section 702
 *   kernel: the constitutional-floor reading, under which the Fourth
 *   Amendment's warrant requirement attaches to every government query that
 *   retrieves U.S. person communications content, regardless of whether the
 *   underlying collection was lawful foreign targeting and regardless of any
 *   statutory minimization regime. On this reading the operative question is
 *   criminal procedure, not foreign-intelligence authorization: a query is a
 *   search, a search requires a warrant supported by individualized probable
 *   cause issued before the query, and the FISA Court is the reviewing forum.
 *   The constraint's costs fall on executive operational speed and secrecy;
 *   its protection accrues to U.S. persons whose content sits in agency
 *   databases. Family note: the kernel decomposes into three readings with
 *   different victim sets and different extractiveness referents — this file
 *   authors only the query-stage warrant floor; the incidental-collection and
 *   foreign-target-strict readings are separate stories with their own
 *   epsilon values, linked through the network section. The claimed type and
 *   the metrics are authored independently: the claim states the structure
 *   judged true; the metrics describe the constraint's actual operation as
 *   measured.
 *
 * KEY AGENTS:
 *   - executive_intelligence_agencies: Primary target (institutional/constrained) — bears the compliance costs of pre-query individualized review
 *   - us_person_communications_subjects: Primary beneficiary (powerless/trapped) — holds the enforceable entitlement the requirement confers
 *   - fisa_court: Administering seat (institutional/constrained) — conducts individualized probable cause review and compliance audit
 *   - national_security_policy_consumers: Secondary target with offsetting benefit (powerful/mobile) — pays in latency, hedges in legitimacy
 *   - civil_liberties_organizations: Organized beneficiary (organized/mobile) — converts the requirement into invokable entitlement
 *   - foreign_intelligence_targets: Excluded party (powerless/trapped) — supplies the database, holds no seat
 *   - pclob: Analytical observer (institutional/analytical) — audits and reports
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__constitutional_floor_reading, 0.25).
domain_priors:suppression_score(fisa_702_statutory_text__constitutional_floor_reading, 0.4).
domain_priors:theater_ratio(fisa_702_statutory_text__constitutional_floor_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__constitutional_floor_reading, tangled_rope).
narrative_ontology:human_readable(fisa_702_statutory_text__constitutional_floor_reading, "Constitutional Floor Reading: Pre-Query Warrant Requirement for 702 Content Queries").
narrative_ontology:topic_domain(fisa_702_statutory_text__constitutional_floor_reading, "legal/constitutional/national_security").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__constitutional_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__constitutional_floor_reading, '1279d003-7afc-4e8e-b55d-3704e0700c6c').
narrative_ontology:cs_kernel_codification('1279d003-7afc-4e8e-b55d-3704e0700c6c', fixed_text).
narrative_ontology:cs_authority_grounding('1279d003-7afc-4e8e-b55d-3704e0700c6c', lineage).
narrative_ontology:cs_interpretation_layer_present('1279d003-7afc-4e8e-b55d-3704e0700c6c').
narrative_ontology:cs_reading_relation('1279d003-7afc-4e8e-b55d-3704e0700c6c', fisa_702_statutory_text__incidental_collection_reading, forecloses).
narrative_ontology:cs_reading_relation('1279d003-7afc-4e8e-b55d-3704e0700c6c', fisa_702_statutory_text__foreign_target_strict_reading, influences).
narrative_ontology:cs_axiom('1279d003-7afc-4e8e-b55d-3704e0700c6c', foundational, query_of_us_person_content_is_search).
narrative_ontology:cs_axiom_status(query_of_us_person_content_is_search, holdable).
narrative_ontology:cs_axiom_grounding('1279d003-7afc-4e8e-b55d-3704e0700c6c', query_of_us_person_content_is_search, deontological).
narrative_ontology:cs_axiom('1279d003-7afc-4e8e-b55d-3704e0700c6c', foundational, foreign_intelligence_purpose_no_exception).
narrative_ontology:cs_axiom_status(foreign_intelligence_purpose_no_exception, holdable).
narrative_ontology:cs_axiom_grounding('1279d003-7afc-4e8e-b55d-3704e0700c6c', foreign_intelligence_purpose_no_exception, conventional).
narrative_ontology:cs_axiom('1279d003-7afc-4e8e-b55d-3704e0700c6c', secondary, fisc_individualized_prequery_review).
narrative_ontology:cs_axiom_status(fisc_individualized_prequery_review, holdable).
narrative_ontology:cs_axiom_grounding('1279d003-7afc-4e8e-b55d-3704e0700c6c', fisc_individualized_prequery_review, instrumental).
narrative_ontology:cs_reference_frame('1279d003-7afc-4e8e-b55d-3704e0700c6c', individualized_probable_cause_baseline).
narrative_ontology:cs_drift_state('1279d003-7afc-4e8e-b55d-3704e0700c6c', contemporary_bulk_collection_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1279d003-7afc-4e8e-b55d-3704e0700c6c', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, us_person_communications_subjects).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, civil_liberties_organizations).
narrative_ontology:constraint_victim(fisa_702_statutory_text__constitutional_floor_reading, executive_intelligence_agencies).
narrative_ontology:constraint_victim(fisa_702_statutory_text__constitutional_floor_reading, national_security_policy_consumers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, national_security_policy_consumers).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__constitutional_floor_reading, fourth_amendment_warrant_clause).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__constitutional_floor_reading, individualized_probable_cause_doctrine).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__constitutional_floor_reading, search_requires_particularized_showing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% U.S. persons whose communications enter 702 collection incidentally when one party to a communication is a targeted non-U.S. person abroad. They cannot consent to or decline this collection, learn of it prospectively, or remove their data from agency holdings. Under the current query regime their content can be retrieved by identifier without any warrant addressed to them; under this reading every retrieval touching their content requires a probable cause warrant issued before the query.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, us_person_communications_subjects, beneficiary,
    powerless, biographical, trapped, national).

% The NSA, FBI, and CIA operate the collection systems and the query tools. They lose direct self-service access to U.S. person content: each query touching such content routes through warrant applications, minimization review, and documented justification, adding latency to time-sensitive operations and putting tradecraft into application records. They cannot abandon their foreign-intelligence mission, and their leadership has spent two decades arguing in sworn testimony and public comment that warrant requirements for queries would blind them to threats. Exit from the requirement is unavailable short of curtailing the program itself.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, executive_intelligence_agencies, payer,
    institutional, biographical, constrained, global).

% White House, National Security Council, and interagency consumers of raw and finished signals intelligence. They bear the latency and granularity costs when analysts must seek warrants instead of running immediate identifier sweeps, and in crisis periods they feel the delay first. They can shift emphasis toward other intelligence disciplines when signals access slows, which softens their exposure relative to the operating agencies. They also hold a longer-run stake in intelligence practices that survive scandal and court challenge intact.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, national_security_policy_consumers, payer,
    powerful, immediate, mobile, national).
narrative_ontology:stakeholder_secondary_role(fisa_702_statutory_text__constitutional_floor_reading, national_security_policy_consumers, beneficiary).

% The Foreign Intelligence Surveillance Court would administer the requirement: receiving warrant applications for U.S.-person-content queries, judging probable cause individually, auditing compliance, and censuring violations. Its docket and its gatekeeping authority over executive access to the database expand accordingly. It acts within statutes and precedent it did not write and cannot shop for a different jurisdiction.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, fisa_court, agenda_setter,
    institutional, generational, constrained, national).

% Litigation and advocacy organizations that have pressed the query-warrant question through amicus filings, reauthorization campaigns, and public reporting. A settled warrant requirement converts their ongoing defensive litigation into an enforceable entitlement they can invoke on behalf of clients, and supplies a doctrinal anchor for adjacent surveillance fights. Their participation is voluntary and they can redirect effort to other fronts if this one closes.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, civil_liberties_organizations, beneficiary,
    organized, generational, mobile, national).

% Non-U.S. persons abroad whose communications with U.S. persons or from targeted facilities constitute the collected database. Every U.S.-person-content query in this debate runs against data taken from them; they have no standing in U.S. courts, no vote in reauthorization, and no seat in the constitutional conversation that decides how their communications are accessed. The warrant requirement changes who inside the United States may examine their traffic and on what showing; it does not change their own exposure to collection.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, foreign_intelligence_targets, excluded,
    powerless, immediate, trapped, global).

% The Privacy and Civil Liberties Oversight Board audits query practices, publishes findings on improper U.S.-person queries, and recommends procedural changes. It holds documents, issues reports, and testifies before Congress; it neither grants nor withholds access and bears none of the compliance burden.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, pclob, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fisa_702_statutory_text__constitutional_floor_reading, fisa_court).
narrative_ontology:fixing_cost_class(fisa_702_statutory_text__constitutional_floor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Replaces agency self-certification with a single uniform gateway for access to U.S. person communications content: individualized probable cause review by a neutral court before any query touches identified U.S. person content, producing access decisions that are documented, auditable, and contestable.
% TRANSFER_FUNCTION: Moves access-authority over U.S. person communications content from executive self-certification to judicial warrant issuance; moves operational speed and secrecy out of unconstrained agency discretion and into recorded process; confers on U.S. persons an enforceable procedural entitlement (a pre-query warrant addressed to their content) they currently lack.
% ABSENT_VOICES: Foreign intelligence targets whose communications constitute the queried database have no standing in U.S. courts and no seat anywhere in this framework. Line analysts whose daily workflow the requirement restructures speak only through agency leadership. Future U.S. persons not yet of record in any database are represented by no one.
% DISAPPEARANCE_RATIONALE: If the requirement vanished overnight, agencies would resume unrestricted content queries against U.S.-person identifiers immediately; the Fourth Amendment's application to digital-era government access would revert to executive self-certification; the FISC's query-review jurisdiction, the compliance-audit apparatus built around query procedures, and the litigation posture of every civil-liberties challenge would dissolve or reorganize around the removed rule.
% FOUNDING_PROBLEM: The eighteenth-century problem the warrant clause was built for: general warrants and writs of assistance — state access to persons' papers without individualized suspicion, neutral review, or particularized limits. This reading recasts warrantless 702 content queries as the digital recurrence of that problem: bulk access to citizens' communications justified by purpose rather than particularized cause.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: PCLOB audit reports documenting FBI query violations against U.S.-person selectors (declassified 2014-2023), declassified FISC opinions censuring improper queries, and the historical record of general warrants assembled by legal historians — none of these sources sits inside the civil-liberties beneficiary set. The executive's own two-decade resistance to the requirement independently attests that the constraint binds something real.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__constitutional_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__constitutional_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__constitutional_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(fisa_702_statutory_text__constitutional_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fisa_702_statutory_text__constitutional_floor_reading, 0.25, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fisa_702_statutory_text__constitutional_floor_reading_tests).
:- end_tests(fisa_702_statutory_text__constitutional_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.25: the requirement's real burden is compliance cost borne by agencies whose speed and secrecy preferences are genuine operational goods — pre-query individualized review adds latency to time-sensitive work and puts tradecraft into application records — but the burden is bounded because lawful paths (individualized warrants, emergency procedures, non-702 collection) remain open. Suppression at 0.40: the rule forecloses warrantless content querying outright and backs compliance with audit findings, funding conditionality, and censure, yet it eliminates no lawful alternative category, so its coercive edge is real but narrow. Theater at 0.15: judicial review under this reading is functional gatekeeping, not ritual — applications carry consequences. Accessibility collapse at 0.55: once the requirement is understood, the warrantless bulk-query option collapses for agencies, but substitute authorities survive, so alternatives degrade rather than vanish. Resistance at 0.65: two decades of sustained executive opposition — testimony against statutory warrant amendments, litigation posture, compliance friction — marks this as a construct that must be defended, not a rule anyone treats as self-executing. The measurement series share one grid (t=0,3,6,9,12,15,18) so every tracked metric is authored at every examined point; the theater series declines as the demand matured from academic argument into operative procedural machinery, while extractiveness and enforcement requirement accumulated as the database and the stakes grew.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the administering seat compute differently from identical structure. From the agencies' position the requirement is an imposed cost center that degrades mission performance and answers to a court they cannot select; from U.S. persons' position it is the difference between content held about them and content accessible about them only on individualized cause; from the court's position it is expanding jurisdiction and institutional centrality. National-security consumers straddle: they pay latency in crisis and bank legitimacy between crises. The engine computes these per-seat classifications from the structural data; nothing in the authored claim adjudicates among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (U.S. person subjects, civil-liberties organizations) drive those seats toward the beneficiary end of directionality; victim declarations (operating agencies, policy consumers) drive those seats toward the target end, amplified by constrained exit for the agencies — they cannot leave the mission — and damped by mobile substitution for policy consumers. The FISA Court declares no beneficiary or victim position; its directionality rides the canonical fallback for its power atom, and its situation notes the jurisdictional gain explicitly so downstream readers see the self-interest. No directionality overrides were needed: role-plus-exit data already separates the seats, and the override surface is keyed by power atom, which would smear corrections across the three institutional seats that need to stay distinct.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy turns on whether the founding problem survives. It does: as long as bulk collection places U.S. person content in agency holdings, the general-warrant analogue — access justified by purpose rather than particularized cause — recurs with every query, so the founding problem is live and the constraint cannot decay into vestige while the database exists. The classification guards both mislabelings: reading the requirement as pure coordination would erase the real, resisted compliance burden agencies bear; reading it as pure extraction would erase the uniform procedural protection that is its actual output. The tangled-rope structure holds both facts. The scalability omega tracks the one path by which the constraint could rot into theater: if individualized review cannot scale, bulk-category warrants would reduce it to rubber-stamp maintenance — a drift the temporal theater series watches for.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the fisa_702_statutory_text kernel: does the Fourth Amendment warrant requirement attach to every query touching U.S. person communications content regardless of how the content was lawfully collected, or does lawful foreign targeting combined with minimization satisfy constitutional reasonableness (the position instantiated by the incidental_collection_reading sibling)?',
    'Supreme Court adoption of a governing standard on the query question, or statutory codification (or rejection) of a query-stage warrant requirement in a 702 reauthorization.',
    'If the incidental-collection sibling prevails, this constraint''s victim set empties — no warrant is owed at query time — and its extractiveness recomputes near zero against the query practice; if this reading prevails, the sibling''s retained-and-queryable U.S. person data becomes constitutionally inaccessible without warrants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the 702 kernel governs the query-stage warrant question.').

omega_variable(
    collection_query_stage_boundary,
    'Does the constitutional event occur at acquisition (so the remedy is stricter targeting and minimization, the foreign_target_strict_reading''s terrain) or at query (so the remedy is individualized warrants, this reading''s terrain)?',
    'Doctrinal analysis of search doctrine''s stage-sensitivity, plus comparative outcomes in regimes that tightened collection versus regimes that warranted queries.',
    'Determines where the remedial burden lands: collection-stage readings shrink the database this reading governs; query-stage readings leave the database intact and gate access. The two sibling strategies are complements rather than rivals only if the boundary is drawn at both stages.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collection_query_stage_boundary, conceptual, 'Whether the constitutional trigger sits at collection or at query.').

omega_variable(
    warrant_compliance_cost_magnitude,
    'What fraction of current foreign-intelligence products and domestic-threat assessments actually depends on warrantless queries of U.S. person communications content, and what delay would individualized pre-query review add?',
    'Declassified query-volume statistics, PCLOB audits, and agency testimony comparing product yield with and without U.S.-person-selector queries.',
    'Sets the true magnitude of epsilon: if critical products collapse without warrantless queries, compliance costs sit at the high end of the measured band and the coordination-extraction trade-off sharpens; if yield is marginal, extraction is mostly preference loss and the coordination reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(warrant_compliance_cost_magnitude, empirical, 'Actual operational dependence on warrantless U.S.-person-content queries.').

omega_variable(
    individualization_scalability,
    'Can individualized probable cause review operate at the scale of current query volume, or does adoption force either bulk-category warrants that hollow the requirement or a drastic reduction in U.S.-person querying?',
    'Pilot programs requiring warrants for subsets of queries, FISC capacity modeling, and throughput comparison with traditional criminal-warrant practice.',
    'If individualization cannot scale, the constraint as specified decays into theatrical review (bulk warrants rubber-stamped) and its classification drifts toward inertial maintenance; if it scales, the constraint holds as enforced procedure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(individualization_scalability, empirical, 'Feasibility of individualized review at operational query volume.').

omega_variable(
    foreign_intel_exception_persistence,
    'Will the judiciary hold the warrant requirement absolute for 702 queries, or carve a foreign-intelligence special-needs exception (the unresolved Keith-era question) that exempts some or all queries?',
    'Supreme Court treatment of the foreign-intelligence exception question in a case squarely raising it.',
    'An exception converts this constraint from a general floor into a patchwork with exempted zones, shrinking its beneficiary set and altering its enforcement profile.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(foreign_intel_exception_persistence, conceptual, 'Durability of the warrant floor against a foreign-intelligence exception.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__constitutional_floor_reading, 0, 18).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t0, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 0, 0.45).
narrative_ontology:measurement_basis(fisa_tr_t0, observed).
narrative_ontology:measurement(fisa_tr_t3, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 3, 0.42).
narrative_ontology:measurement_basis(fisa_tr_t3, observed).
narrative_ontology:measurement(fisa_tr_t6, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 6, 0.36).
narrative_ontology:measurement_basis(fisa_tr_t6, observed).
narrative_ontology:measurement(fisa_tr_t9, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 9, 0.31).
narrative_ontology:measurement_basis(fisa_tr_t9, observed).
narrative_ontology:measurement(fisa_tr_t12, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement_basis(fisa_tr_t12, observed).
narrative_ontology:measurement(fisa_tr_t15, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement_basis(fisa_tr_t15, observed).
narrative_ontology:measurement(fisa_tr_t18, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 18, 0.15).
narrative_ontology:measurement_basis(fisa_tr_t18, observed).

% Extraction over time
narrative_ontology:measurement(fisa_be_t0, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement_basis(fisa_be_t0, observed).
narrative_ontology:measurement(fisa_be_t3, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 3, 0.13).
narrative_ontology:measurement_basis(fisa_be_t3, observed).
narrative_ontology:measurement(fisa_be_t6, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 6, 0.17).
narrative_ontology:measurement_basis(fisa_be_t6, observed).
narrative_ontology:measurement(fisa_be_t9, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 9, 0.2).
narrative_ontology:measurement_basis(fisa_be_t9, observed).
narrative_ontology:measurement(fisa_be_t12, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 12, 0.22).
narrative_ontology:measurement_basis(fisa_be_t12, observed).
narrative_ontology:measurement(fisa_be_t15, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 15, 0.24).
narrative_ontology:measurement_basis(fisa_be_t15, observed).
narrative_ontology:measurement(fisa_be_t18, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 18, 0.25).
narrative_ontology:measurement_basis(fisa_be_t18, observed).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t0, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(fisa_su_t0, observed).
narrative_ontology:measurement(fisa_su_t3, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 3, 0.19).
narrative_ontology:measurement_basis(fisa_su_t3, observed).
narrative_ontology:measurement(fisa_su_t6, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 6, 0.27).
narrative_ontology:measurement_basis(fisa_su_t6, observed).
narrative_ontology:measurement(fisa_su_t9, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 9, 0.32).
narrative_ontology:measurement_basis(fisa_su_t9, observed).
narrative_ontology:measurement(fisa_su_t12, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 12, 0.35).
narrative_ontology:measurement_basis(fisa_su_t12, observed).
narrative_ontology:measurement(fisa_su_t15, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 15, 0.38).
narrative_ontology:measurement_basis(fisa_su_t15, observed).
narrative_ontology:measurement(fisa_su_t18, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 18, 0.4).
narrative_ontology:measurement_basis(fisa_su_t18, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__constitutional_floor_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, incidental_collection_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, foreign_target_strict_reading).

% DUAL FORMULATION NOTE:
% The colloquial label '702 and the Fourth Amendment' conflates three structurally distinct claims: (1) whether queries of U.S. person content are Fourth Amendment searches requiring pre-query warrants (this file, constitutional_floor_reading); (2) whether warrantless retention and querying of incidentally collected U.S. person content is permissible under a foreign-intelligence justification (incidental_collection_reading); (3) whether the collection stage itself must be confined so strictly that incidental U.S. person data never accumulates (foreign_target_strict_reading). Each claim has its own epsilon, its own victim set, and its own failure modes; they are linked here because this reading forecloses the incidental reading's core premise within any single legal framework and structurally pressures the strict reading by raising the cost of incidental accumulation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
