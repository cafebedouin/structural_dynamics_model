% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__constitutional_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Constitutional Floor Reading — Warrant Requirement for Section 702 Queries
 *   domain: constitutional/national_security
 *
 * SUMMARY:
 *   This story instantiates the constitutional_floor_reading of the Section
 *   702 statutory-text kernel: the claim that, regardless of what the statute
 *   or its procedures permit, the Fourth Amendment independently requires a
 *   probable-cause warrant before the government searches U.S.-person
 *   communications content — and that agency queries of 702-collected content
 *   are such searches, wherever the sender sat and however lawful the
 *   underlying collection. Under this reading the operative question is
 *   criminal procedure, not foreign-intelligence policy: each query touching
 *   U.S.-person content requires a prior, particularized, judicially issued
 *   warrant, with the Foreign Intelligence Surveillance Court conducting
 *   individualized review. The constraint this story classifies is that
 *   warrant floor itself — a standing constitutional command binding every
 *   agency query practice. Its extractiveness is authored as the reading
 *   assesses it: the compliance burden the floor imposes on executive speed
 *   and secrecy preferences (epsilon 0.25), not the benefits of the privacy
 *   it secures and not any sibling reading's structure. The story links to
 *   its two sibling readings as a constraint family.
 *
 * KEY AGENTS:
 *   - fisa_court_judges: agenda-setting adjudicator (institutional/constrained) — administers individualized probable-cause review; docket transforms from programmatic oversight to warrant adjudication
 *   - federal_intelligence_agencies: primary payer (institutional/trapped) — NSA, FBI, and CIA bear the warrant step on every U.S.-person content query; no exit short of constitutional amendment
 *   - intelligence_community_analysts: operational payer (moderate/mobile) — absorb workflow friction and delay at the query console
 *   - us_persons: primary beneficiary (powerless-diffuse/trapped) — the protected class receiving prior neutral review; access exercised through intermediaries
 *   - civil_liberties_organizations: organized beneficiary-advocate (organized/mobile) — litigate the reading and benchmark agency compliance
 *   - congressional_intelligence_committees: excluded interpretive authority (institutional/constrained) — oversight displaced by the constitutional override
 *   - foreign_intelligence_targets: excluded class (powerless/trapped) — outside the reading's protective boundary entirely
 *   - constitutional_law_scholars: analytical observer (analytical/analytical) — map the doctrine the litigants borrow
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__constitutional_floor_reading, 0.25).
domain_priors:suppression_score(fisa_702_statutory_text__constitutional_floor_reading, 0.3).
domain_priors:theater_ratio(fisa_702_statutory_text__constitutional_floor_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__constitutional_floor_reading, rope).
narrative_ontology:human_readable(fisa_702_statutory_text__constitutional_floor_reading, "Constitutional Floor Reading — Warrant Requirement for Section 702 Queries").
narrative_ontology:topic_domain(fisa_702_statutory_text__constitutional_floor_reading, "constitutional/national_security").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__constitutional_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__constitutional_floor_reading, '5ce8abbf-180b-425e-89d7-ab49b5c8bf24').
narrative_ontology:cs_kernel_codification('5ce8abbf-180b-425e-89d7-ab49b5c8bf24', fixed_text).
narrative_ontology:cs_authority_grounding('5ce8abbf-180b-425e-89d7-ab49b5c8bf24', lineage).
narrative_ontology:cs_interpretation_layer_present('5ce8abbf-180b-425e-89d7-ab49b5c8bf24').
narrative_ontology:cs_reading_relation('5ce8abbf-180b-425e-89d7-ab49b5c8bf24', fisa_702_statutory_text__incidental_collection_reading, forecloses).
narrative_ontology:cs_reading_relation('5ce8abbf-180b-425e-89d7-ab49b5c8bf24', fisa_702_statutory_text__foreign_target_strict_reading, influences).
narrative_ontology:cs_axiom('5ce8abbf-180b-425e-89d7-ab49b5c8bf24', foundational, warrant_required_for_us_person_content_queries).
narrative_ontology:cs_axiom_status(warrant_required_for_us_person_content_queries, holdable).
narrative_ontology:cs_axiom_grounding('5ce8abbf-180b-425e-89d7-ab49b5c8bf24', warrant_required_for_us_person_content_queries, deontological).
narrative_ontology:cs_axiom('5ce8abbf-180b-425e-89d7-ab49b5c8bf24', foundational, foreign_intelligence_purpose_no_search_exception).
narrative_ontology:cs_axiom_status(foreign_intelligence_purpose_no_search_exception, holdable).
narrative_ontology:cs_axiom_grounding('5ce8abbf-180b-425e-89d7-ab49b5c8bf24', foreign_intelligence_purpose_no_search_exception, deontological).
narrative_ontology:cs_reference_frame('5ce8abbf-180b-425e-89d7-ab49b5c8bf24', individualized_probable_cause_baseline).
narrative_ontology:cs_drift_state('5ce8abbf-180b-425e-89d7-ab49b5c8bf24', post_carpenter_risaa_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5ce8abbf-180b-425e-89d7-ab49b5c8bf24', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, us_persons).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, civil_liberties_organizations).
narrative_ontology:constraint_victim(fisa_702_statutory_text__constitutional_floor_reading, federal_intelligence_agencies).
narrative_ontology:constraint_victim(fisa_702_statutory_text__constitutional_floor_reading, intelligence_community_analysts).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__constitutional_floor_reading, fourth_amendment_warrant_clause).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__constitutional_floor_reading, katz_reasonable_expectation_privacy).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__constitutional_floor_reading, carpenter_digital_content_protection).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sit on the Foreign Intelligence Surveillance Court, the tribunal that would conduct individualized probable-cause review of each government request to query U.S.-person communications content under this reading. Today they approve targeting and procedures; under the reading their docket becomes case-by-case warrant adjudication with sworn applications, particularity, and record-keeping. They cannot decline the jurisdiction the Constitution assigns them, and their institutional posture shifts from programmatic overseer toward criminal-procedure trial-court analogue.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, fisa_court_judges, agenda_setter,
    institutional, generational, constrained, national).

% The NSA, FBI, and CIA operate the 702 collection and query infrastructure. Under this reading every query touching U.S.-person content requires a prior judicially issued warrant supported by probable cause: applications, particularized descriptions of targets, expedited-review lanes for time-sensitive operations, and auditable compliance trails. They retain foreign-targeting collection and emergency authorities but lose the ability to self-certify access to U.S.-person content. Exit from the obligation is unavailable short of constitutional amendment; their adaptation space is procedural.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, federal_intelligence_agencies, payer,
    institutional, generational, trapped, national).

% Front-line personnel whose daily work runs database queries against collected communications. The reading inserts a warrant step between their analytic question and the answer whenever U.S.-person content is involved: case-initiation paperwork, waiting periods, narrower follow-on queries. Career mobility into the private sector exists, but within the job the workflow change is unavoidable and is felt individually as friction and delay.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, intelligence_community_analysts, payer,
    moderate, biographical, mobile, national).

% The statutorily defined protected class — citizens, lawful permanent residents, and U.S.-person corporations — whose communications content is collected incidentally to foreign targeting and queried without individualized warrants under current practice. They receive prior neutral review before government access to their content. No individual can opt out of the protection or out of the collection that precedes it; the benefit arrives as a class-wide legal shield exercised mostly through intermediaries.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, us_persons, beneficiary,
    powerless, generational, trapped, national).

% Litigating and advocacy organizations that pressed this reading in amicus briefs, comment campaigns, and suits over query practices. They gain a doctrinal anchor: a concrete constitutional rule to enforce, standing theories built on queried members, and a benchmark against which agency compliance is measured. They choose their engagements and can withdraw from a fight without losing the protection itself.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, civil_liberties_organizations, beneficiary,
    organized, generational, mobile, national).

% The House and Senate committees that oversee 702 and wrote its procedures. Because the reading operates regardless of statutory interpretation, their interpretive settlements — minimization rules, query standards, authorization conditions — no longer decide the constitutional question; their role narrows to funding, confirmation, and amendment politics. They retain formal power over the statute but not over the floor the reading sets.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, congressional_intelligence_committees, excluded,
    institutional, biographical, constrained, national).

% Non-U.S. persons abroad whose communications are the program's collection objective. The reading's shield stops at the U.S.-person boundary: they receive no warrant protection, no forum, and no notice, and their communications remain collectible and usable as before. They are the class whose exclusion defines the reading's reach, and no institution represents their objection inside the process.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, foreign_intelligence_targets, excluded,
    powerless, generational, trapped, global).

% Academic commentators and former officials who map the doctrine: how Katz, Miller, Smith, Carpenter, and the special-needs and foreign-surveillance cases bear on query-stage searches. They publish the frameworks the litigants borrow, testify in Congress, and track the compliance record, but hold no decision power over any of it.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, constitutional_law_scholars, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fisa_702_statutory_text__constitutional_floor_reading, fisa_court_judges).
narrative_ontology:fixing_cost_class(fisa_702_statutory_text__constitutional_floor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the executive-self-certification problem in government access to private communications: instead of each agency deciding for itself when it may read U.S.-person content, every access decision is routed through a neutral magistrate applying a shared probable-cause standard, producing uniform process, auditable records, and a common evidentiary foundation for downstream use.
% TRANSFER_FUNCTION: Moves decision authority over U.S.-person content access from agency self-certification to judicial determination; moves operating costs (application drafting, delay, disclosure of investigative interest) from the public's privacy sphere onto agency budgets and timelines; and moves the risk of improper querying from diffuse, unnoticed individuals to concentrated, reviewable case files.
% ABSENT_VOICES: Foreign intelligence targets have no seat anywhere in the process — the reading's protection ends at the U.S.-person line, and no institution speaks for them inside it. Congressional intelligence committees are heard on funding and amendment but displaced from the interpretive question the reading removes from statute. Agency operators sit in the room only as applicants, not as adjudicators of the standard's breadth.
% DISAPPEARANCE_RATIONALE: Remove the warrant floor overnight and agency self-certification of U.S.-person content queries resumes immediately: query volumes expand to analytic convenience, the court's query docket empties, civil-liberties litigation refocuses on re-establishing the rule, and downstream criminal use of query results loses its constitutional filter — every named seat's position rearranges around the returned discretion.
% FOUNDING_PROBLEM: The general-warrant evil the Fourth Amendment was written against: executive officers searching private papers on their own authority, without prior neutral review, on the strength of their own certification that the search serves a public purpose — re-instantiated at database scale when the same officers query stored communications content.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the beneficiary set: the Supreme Court's warrant-clause line from Katz through Carpenter repeatedly names self-certified digital searches as the recurring danger; the Church Committee's 1975–76 findings documented warrantless surveillance abuse from a congressional seat; the Privacy and Civil Liberties Oversight Board's 702 report and the FISC's own opinions on agency query compliance record the enforcement gap from oversight positions. The agencies that bear the rule's costs uniformly do not corroborate — their consistent testimony is that statutory procedures alone manage the problem.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__constitutional_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__constitutional_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__constitutional_floor_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
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
 *   Scores are authored independently of the rope claim. Extractiveness sits at 0.25 because the floor's burden on the executive is real but bounded: expedited and emergency warrant lanes, retained foreign-targeting collection, and preserved non-content tools mean the rule channels rather than disables. Suppression is 0.30 — and per the framework's rule it stays a raw structural property, unscaled by power or scope: the floor coerces warrantless queriers only modestly because lawful alternatives remain open. Theater is 0.15: individualized review is functional adjudication, with residual ceremony in application formality. Accessibility collapse is 0.20: once the floor is understood, the alternative path (obtain a warrant) remains fully workable, so alternatives do not collapse. Resistance is 0.60: sustained agency opposition, recurring query-compliance controversies, and reauthorization fights show the construct must be actively defended. The temporal series share one eight-point grid (2008–2026): theater declines as review functionalizes, the compliance burden rises as doctrine extends toward query-stage searches, and suppression_requirement rises as the enforcement machinery the floor requires matures — an enforcement build-out, not decay. Receipt note: the arrangement's gains — adjudicatory authority over a new docket — accrue to the fisa_court_judges seat, hence gain_flow names it; removing the floor would require a constitutional amendment, hence fixing_cost is prohibitive.
 *
 * PERSPECTIVAL GAP:
 *   Seats diverge sharply. From the agency seat the floor is a tax on operational tempo imposed by a court that does not carry the mission; computed from victim declaration, trapped exit, and institutional power, that seat lands near the full-target end. From the us_persons seat the same structure is the difference between self-certified access and neutral review — a subsidy arriving as a class-wide shield, near the beneficiary end despite zero individual agency. The analyst seat experiences friction without owning the policy; the bench experiences mandate without bearing the mission cost; the scholar seat sees structure without stakes. Same constraint, differently experienced per seat — the engine computes the divergence from the structural data; the rope claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (us_persons, civil_liberties_organizations) drive those seats toward the beneficiary end — damped effective burden, with us_persons pushed furthest toward pure subsidy by their trapped, no-opt-out position. Victim declarations (federal_intelligence_agencies, intelligence_community_analysts) drive those seats toward the target end, with the agencies pushed furthest by trapped exit and institutional scope. The FISC bench is declared neither beneficiary nor victim: it administers, and its directionality derives from institutional power and constrained exit — mid-range, with the receipt of adjudicatory authority recorded on the gain-flow surface rather than folded into its position. Excluded seats (congressional committees, foreign targets) shape the constraint's boundary without collecting from it or paying into it. No directionality overrides were needed: the beneficiary/victim declarations plus exit options reproduce the structural relationships faithfully.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — executive self-certification of searches, the general-warrant evil — is live, so no mandatrophy declaration is made and none is due. The classification work cuts both ways: calling the floor a rope keeps its genuine coordination function (uniform neutral process replacing self-certification) from being misread as pure confiscation of agency capability, while keeping the 0.25 compliance burden on the books prevents the coordination label from laundering the real cost the executive bears. The degraded-institution failure mode is distant: the floor's function strengthens as doctrine extends toward query-stage searches, the opposite of atrophy. If the founding problem were ever genuinely solved — if self-certified access lost all institutional support — the floor would persist as settled constitutional furniture, at which point the rope-versus-fixed-structure boundary question, not mandate obsolescence, would be the live ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position_fisa_702,
    'This constraint is one reading of the fisa_702_statutory_text kernel — the constitutional_floor_reading. What would the sibling readings change structurally, and where exactly does the disagreement sit?',
    'Supreme Court adjudication of query-stage search status reaching the merits, or a direct constitutional confrontation case; until then the sibling readings remain separate constraint stories with their own epsilon and victim structures.',
    'If incidental_collection_reading prevailed, this story''s warrant machinery dissolves and the victim set relocates to U.S. persons queried without process; if foreign_target_strict_reading prevailed, the constraint point moves upstream to collection-stage minimization and this reading survives only as a residual backstop. The disagreement is located at two joints: whether a database query of already-collected U.S.-person content is itself a Fourth Amendment search, and whether foreign-intelligence purpose is a categorical exception to the warrant clause.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position_fisa_702, conceptual, 'Committer structure: one of three readings of the 702 statutory-text kernel; siblings are separate stories.').

omega_variable(
    carpenter_extension_open_question,
    'Do Carpenter''s principles reach query-stage access to stored communications content collected under Section 702, making this reading''s central premise imminent positive law rather than aspiration?',
    'Pending and future litigation over 702 queries reaching appellate merits; district-court treatments of query-stage searches post-Carpenter; evolution of Justice Department internal query standards.',
    'An affirmative answer converts the reading from advocacy position to governing law, driving the measured compliance burden toward its full designed weight; a negative answer strands the reading as a minority doctrinal position with reduced enforcement traction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carpenter_extension_open_question, empirical, 'Whether doctrine extends to query-stage searches of 702-collected content.').

omega_variable(
    warrant_compliance_cost_magnitude,
    'What is the true operational cost of individualized pre-query warrants — do expedited and emergency review lanes preserve most analytic capability, as this reading assumes, or does case-by-case review degrade foreign-intelligence throughput as the agencies claim?',
    'Declassified processing-time and approval-rate data from comparable warrant regimes (Title I FISA orders, criminal wiretap intercepts); pilot programs with expedited lanes; Privacy and Civil Liberties Oversight Board-style study.',
    'Low realized costs would shrink the constraint''s extractive footprint below 0.25 and recast executive resistance as preference rather than necessity; high realized costs would confirm the 0.25 estimate and strengthen proportionality arguments against the reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(warrant_compliance_cost_magnitude, empirical, 'Magnitude of the compliance burden the warrant floor actually imposes on operations.').

omega_variable(
    us_person_boundary_principle,
    'Is this reading''s U.S.-person boundary a principled constitutional limit (membership-based protection) or a jurisdictional artifact that under-protects identically situated non-U.S. persons?',
    'Comparative doctrine (European Court of Human Rights privacy jurisprudence, ICCPR state practice on extraterritorial privacy); doctrinal evolution on extraterritorial Fourth Amendment application; treaty and executive-agreement development.',
    'If the boundary is artifact rather than principle, the reading''s beneficiary structure is incomplete and its moral authority weakens against expansion challenges; if principled, the boundary stabilizes the reading''s scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_person_boundary_principle, conceptual, 'Status of the U.S.-person boundary within the reading''s protective structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__constitutional_floor_reading, 2008, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t2008, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2008, 0.42).
narrative_ontology:measurement_basis(fisa_tr_t2008, observed).
narrative_ontology:measurement(fisa_tr_t2011, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2011, 0.38).
narrative_ontology:measurement_basis(fisa_tr_t2011, observed).
narrative_ontology:measurement(fisa_tr_t2013, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2013, 0.34).
narrative_ontology:measurement_basis(fisa_tr_t2013, observed).
narrative_ontology:measurement(fisa_tr_t2015, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2015, 0.27).
narrative_ontology:measurement_basis(fisa_tr_t2015, observed).
narrative_ontology:measurement(fisa_tr_t2018, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2018, 0.22).
narrative_ontology:measurement_basis(fisa_tr_t2018, observed).
narrative_ontology:measurement(fisa_tr_t2020, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2020, 0.19).
narrative_ontology:measurement_basis(fisa_tr_t2020, observed).
narrative_ontology:measurement(fisa_tr_t2024, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2024, 0.16).
narrative_ontology:measurement_basis(fisa_tr_t2024, observed).
narrative_ontology:measurement(fisa_tr_t2026, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2026, 0.15).
narrative_ontology:measurement_basis(fisa_tr_t2026, projected).

% Extraction over time
narrative_ontology:measurement(fisa_be_t2008, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2008, 0.06).
narrative_ontology:measurement_basis(fisa_be_t2008, observed).
narrative_ontology:measurement(fisa_be_t2011, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2011, 0.1).
narrative_ontology:measurement_basis(fisa_be_t2011, observed).
narrative_ontology:measurement(fisa_be_t2013, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2013, 0.14).
narrative_ontology:measurement_basis(fisa_be_t2013, observed).
narrative_ontology:measurement(fisa_be_t2015, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2015, 0.18).
narrative_ontology:measurement_basis(fisa_be_t2015, observed).
narrative_ontology:measurement(fisa_be_t2018, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2018, 0.21).
narrative_ontology:measurement_basis(fisa_be_t2018, observed).
narrative_ontology:measurement(fisa_be_t2020, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2020, 0.23).
narrative_ontology:measurement_basis(fisa_be_t2020, observed).
narrative_ontology:measurement(fisa_be_t2024, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2024, 0.24).
narrative_ontology:measurement_basis(fisa_be_t2024, observed).
narrative_ontology:measurement(fisa_be_t2026, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2026, 0.25).
narrative_ontology:measurement_basis(fisa_be_t2026, projected).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t2008, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2008, 0.05).
narrative_ontology:measurement_basis(fisa_su_t2008, observed).
narrative_ontology:measurement(fisa_su_t2011, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2011, 0.1).
narrative_ontology:measurement_basis(fisa_su_t2011, observed).
narrative_ontology:measurement(fisa_su_t2013, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2013, 0.18).
narrative_ontology:measurement_basis(fisa_su_t2013, observed).
narrative_ontology:measurement(fisa_su_t2015, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2015, 0.26).
narrative_ontology:measurement_basis(fisa_su_t2015, observed).
narrative_ontology:measurement(fisa_su_t2018, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2018, 0.33).
narrative_ontology:measurement_basis(fisa_su_t2018, observed).
narrative_ontology:measurement(fisa_su_t2020, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2020, 0.38).
narrative_ontology:measurement_basis(fisa_su_t2020, observed).
narrative_ontology:measurement(fisa_su_t2024, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2024, 0.44).
narrative_ontology:measurement_basis(fisa_su_t2024, observed).
narrative_ontology:measurement(fisa_su_t2026, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2026, 0.48).
narrative_ontology:measurement_basis(fisa_su_t2026, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__constitutional_floor_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__incidental_collection_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__foreign_target_strict_reading).

% DUAL FORMULATION NOTE:
% Constraint family: one kernel (the Section 702 statutory text), three readings instantiating structurally distinct constraints per the epsilon-invariance decomposition. This member (constitutional_floor_reading) locates the constraint at query-stage access and authors epsilon as the warrant floor's compliance cost on executive speed/secrecy preferences (0.25), with U.S. persons as beneficiaries and the intelligence apparatus as payers. incidental_collection_reading locates the constraint at the same queries but permits them, relocating victims to U.S. persons queried without process; foreign_target_strict_reading moves the constraint point upstream to collection-stage targeting and minimization. Upstream/downstream: the constitutional floor pressures the strict-collection reading (minimization becomes the mechanism that avoids warrant overhead) while logically excluding the warrantless-query reading within any single constitutional framework. Family members are linked via affects_constraints using full composite constraint ids; cs_structure.reading_relations uses the bare reading ids as declared in the kernel context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
