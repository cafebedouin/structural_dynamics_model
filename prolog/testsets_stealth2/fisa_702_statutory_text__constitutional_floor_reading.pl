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
 *   human_readable: Constitutional Warrant Floor for 702 Queries of U.S. Person Communications Content
 *   domain: constitutional law/national security/surveillance policy
 *
 * SUMMARY:
 *   Section 702 of FISA (50 U.S.C. 1881a) authorizes the government to target
 *   non-U.S. persons located abroad and thereby sweeps in large volumes of
 *   U.S. persons' communications; agencies then query the stored content
 *   hundreds of thousands of times a year under self-certified minimization
 *   procedures, without prior judicial cause. The colloquial label '702
 *   safeguards for U.S. persons' covers three structurally distinct claims,
 *   and per the epsilon-invariance principle this corpus models them as a
 *   constraint family rather than one story with a measurement parameter.
 *   This file instantiates the constitutional_floor_reading: the Fourth
 *   Amendment's Warrant Clause supplies a categorical procedural floor for
 *   ANY government search of U.S. person communications content, so 702
 *   queries are searches triggering the individualized probable cause warrant
 *   requirement independent of the foreign/domestic distinction and
 *   independent of how the underlying collection was authorized. The
 *   structural reframing is deliberate: this is a criminal procedure question
 *   wearing a foreign intelligence statute's clothes. Epsilon's referent is
 *   the standing arrangement under contest (warrantless querying under
 *   self-certified minimization), assessed by this reading's own lights; the
 *   reading prices the arrangement's defining feature, access without prior
 *   neutral cause, as the defect, and prices the floor it would impose as a
 *   real but bounded compliance burden on executive operations (epsilon
 *   approximately 0.25, drawn from executive speed and secrecy preferences
 *   foregone). The sibling stories price the same standing arrangement
 *   differently: the incidental_collection_reading prices it near zero, the
 *   foreign_target_strict_reading prices collection-stage overbreadth. Those
 *   differences are properties of the readings, not of one observable.
 *
 * KEY AGENTS:
 *   - executive_intelligence_agencies: Primary payer (institutional/constrained) — bears the floor's concentrated compliance costs: delay, individualized probable cause showings, disclosure, loss of bulk-query speed
 *   - us_person_communicants: Primary beneficiary (powerless/trapped) — hold the privacy interest the floor protects; cannot exit incidental collection into 702 databases
 *   - article_iii_judiciary: Agenda-setter (institutional/analytical) — authors the authoritative answer on search status and required process
 *   - fisa_court_judges: Agenda-setter (institutional/analytical) — would administer the individualized probable cause review the reading requires
 *   - civil_liberties_organizations: Secondary beneficiary (organized/mobile) — litigate and publish the floor into existence
 *   - criminal_defense_bar: Secondary beneficiary (organized/mobile) — converts the floor into suppression leverage in prosecutions
 *   - congress_intelligence_committees: Agenda-setter (institutional/constrained) — funds and codifies review but cannot legislate below the asserted floor
 *   - foreign_surveillance_targets: Excluded (powerless/trapped) — the subjects of the queried corpus, without a seat in U.S. constitutional adjudication
 *   - national_security_law_scholars: Analytical observer — maps the doctrinal terrain, collects nothing, pays nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__constitutional_floor_reading, 0.25).
domain_priors:suppression_score(fisa_702_statutory_text__constitutional_floor_reading, 0.45).
domain_priors:theater_ratio(fisa_702_statutory_text__constitutional_floor_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(fisa_702_statutory_text__constitutional_floor_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__constitutional_floor_reading, tangled_rope).
narrative_ontology:human_readable(fisa_702_statutory_text__constitutional_floor_reading, "Constitutional Warrant Floor for 702 Queries of U.S. Person Communications Content").
narrative_ontology:topic_domain(fisa_702_statutory_text__constitutional_floor_reading, "constitutional law/national security/surveillance policy").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__constitutional_floor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__constitutional_floor_reading, '719e2dd7-fdf1-4c62-9047-2f1f4e8241c2').
narrative_ontology:cs_kernel_codification('719e2dd7-fdf1-4c62-9047-2f1f4e8241c2', fixed_text).
narrative_ontology:cs_authority_grounding('719e2dd7-fdf1-4c62-9047-2f1f4e8241c2', lineage).
narrative_ontology:cs_interpretation_layer_present('719e2dd7-fdf1-4c62-9047-2f1f4e8241c2').
narrative_ontology:cs_reading_relation('719e2dd7-fdf1-4c62-9047-2f1f4e8241c2', fisa_702_statutory_text__incidental_collection_reading, forecloses).
narrative_ontology:cs_reading_relation('719e2dd7-fdf1-4c62-9047-2f1f4e8241c2', fisa_702_statutory_text__foreign_target_strict_reading, coexists_with).
narrative_ontology:cs_axiom('719e2dd7-fdf1-4c62-9047-2f1f4e8241c2', foundational, content_search_requires_individualized_warrant).
narrative_ontology:cs_axiom_status(content_search_requires_individualized_warrant, holdable).
narrative_ontology:cs_axiom_grounding('719e2dd7-fdf1-4c62-9047-2f1f4e8241c2', content_search_requires_individualized_warrant, deontological).
narrative_ontology:cs_axiom('719e2dd7-fdf1-4c62-9047-2f1f4e8241c2', foundational, search_status_independent_of_target_geography).
narrative_ontology:cs_axiom_status(search_status_independent_of_target_geography, holdable).
narrative_ontology:cs_axiom_grounding('719e2dd7-fdf1-4c62-9047-2f1f4e8241c2', search_status_independent_of_target_geography, deontological).
narrative_ontology:cs_reference_frame('719e2dd7-fdf1-4c62-9047-2f1f4e8241c2', warrant_clause_categorical_floor).
narrative_ontology:cs_drift_state('719e2dd7-fdf1-4c62-9047-2f1f4e8241c2', contemporary_post_risaa, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('719e2dd7-fdf1-4c62-9047-2f1f4e8241c2', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, us_person_communicants).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, civil_liberties_organizations).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__constitutional_floor_reading, criminal_defense_bar).
narrative_ontology:constraint_victim(fisa_702_statutory_text__constitutional_floor_reading, executive_intelligence_agencies).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__constitutional_floor_reading, fourth_amendment_warrant_clause).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__constitutional_floor_reading, individualized_probable_cause_review).
narrative_ontology:constraint_vindicates(fisa_702_statutory_text__constitutional_floor_reading, judicial_supervision_of_executive_searches).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides whether 702 queries are Fourth Amendment searches and what process they require. The D.C. Circuit in CDT v. Trump already classified the queries as searches while excusing warrants under the special-needs doctrine; a merits ruling on the warrant question either restructures the entire query regime or entrenches the reasonableness excuse. The courts take no resources from the arrangement and bear none of its operational burdens; their output is doctrine.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, article_iii_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% Serve fourteen-year staggered terms reviewing government applications under FISA, currently ex parte and focused on targeting and minimization procedures. Under this reading they would conduct individualized probable cause review before each query into U.S. person communications content. Their docket size, staffing, clerk resources, and the amicus mechanism determine how much individualized review is practically possible; the 2024 reauthorization expanded their procedural toolkit.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, fisa_court_judges, agenda_setter,
    institutional, generational, analytical, national).

% Run Section 702 collection and query the acquired data hundreds of thousands of times a year for foreign intelligence purposes and, in the FBI's case, for investigations reaching U.S. soil. Under this reading, every query touching U.S. person content would require a prior individualized probable cause showing before a magistrate or the FISA Court. They describe self-certified querying as essential to speed and to protecting sources and methods, and their compliance record (documented query-rule violations in FBI systems) is the opposition's chief exhibit. Their options are warrant applications, narrower query terms, other legal authorities, or seeking statutory change; leaving the constitutional system itself is not on the menu.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, executive_intelligence_agencies, payer,
    institutional, generational, constrained, global).

% People inside the United States whose calls, emails, and messages enter 702 databases when they communicate with targeted foreigners abroad. They never consented, cannot learn what was taken, and cannot opt out of incidental collection. Under this reading they hold a personal constitutional interest: no government examination of their content without a warrant issued on probable cause, regardless of where the person on the other end of the message sits.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, us_person_communicants, beneficiary,
    powerless, biographical, trapped, national).

% Litigate, publish, and lobby for the warrant requirement's application to 702 queries; amicus appearances in CDT v. Trump and successor cases are their main lever, alongside coalition testimony in reauthorization fights. Their issue portfolios are broad, so they can redirect effort if this front closes, and their institutional standing and fundraising grow with each doctrinal win.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, civil_liberties_organizations, beneficiary,
    organized, generational, mobile, national).

% Defend clients whose prosecutions increasingly rest on evidence traceable to 702-derived databases. A warrant requirement hands them a suppression motion wherever the evidentiary chain lacks one; they have begun filing challenges to query-derived evidence and would gain a routine procedural tool under this reading. The tool transfers across clients and case types, so their stake survives any single loss.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, criminal_defense_bar, beneficiary,
    organized, biographical, mobile, national).

% Write and reauthorize Section 702, fund the program, and codify query safeguards as in the 2024 reauthorization. They cannot legislate below a constitutional floor of the kind this reading asserts, but they determine how much individualized review the statute funds and mandates, and their hearings supply the public record both sides argue from. Members split across the readings, which is why the codified safeguards stop short of warrants.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, congress_intelligence_committees, agenda_setter,
    institutional, generational, constrained, national).

% Non-U.S. persons abroad whose communications are the collection's object and whose exchanges with U.S. contacts populate the query corpus. Current doctrine gives them no Fourth Amendment rights and no U.S. forum in which to claim one. This reading regulates only what happens after their data reaches U.S. analysts, leaving their exposure to collection itself unchanged; they would object that the entire debate treats their content as a resource whose handling, not whose taking, is the only question on the table.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, foreign_surveillance_targets, excluded,
    powerless, biographical, trapped, global).

% Map the doctrinal terrain: the third-party doctrine, the special-needs and foreign-intelligence exceptions, the text-history-and-tradition turn, and the administrative-feasibility arguments all bear on whether the reading holds. Their journals, amicus work, and congressional testimony frame the questions courts eventually answer. They take no side's resources and bear none of the burdens; their output is analysis.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__constitutional_floor_reading, national_security_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fisa_702_statutory_text__constitutional_floor_reading, diffuse).
narrative_ontology:fixing_cost_class(fisa_702_statutory_text__constitutional_floor_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, judicially supervised procedure through which executive agencies access U.S. person communications content: individualized probable cause determinations replace each agency's self-certification, giving every query a uniform legitimacy basis that survives administrations and sustains the program's political durability.
% TRANSFER_FUNCTION: Moves decision-making authority over access to U.S. person communications from executive self-certification to judicial warrant issuance; moves operational speed and secrecy from intelligence agencies into a documented, reviewable record; confers enforceable privacy protection on U.S. persons whose content is queried.
% ABSENT_VOICES: Foreign intelligence targets whose communications form the queried corpus have no Fourth Amendment standing and no seat in U.S. adjudication; rank-and-file analysts operating under productivity pressure appear only through Inspector General findings rather than as witnesses; the FISA Court hears the government ex parte except where amicus provisions force a second voice. Each absent seat would alter the record: the targets would contest the framing of their content as a governable resource, and the analysts would testify to the operational cost the principals describe abstractly.
% DISAPPEARANCE_RATIONALE: If the constitutional floor vanished overnight, agencies would revert to self-certified bulk querying within an operating cycle, which is the documented pre-2013 default; the defense bar would lose the suppression theory now forming around query-derived evidence; the FISA Court's role would contract back to program-level review; and communicants' content would become searchable on internal say-so. Numerous dependents sit on both sides of the ledger, so removal rearranges the arrangement rather than settling anything.
% FOUNDING_PROBLEM: General warrants and writs of assistance: executive officers searching whom they pleased, on their own certification, without prior neutral cause. The Warrant Clause was adopted to replace self-certified search with individualized judicial determination based on particularized facts.
% FOUNDING_PROBLEM_CORROBORATION: Attested from outside the benefiting parties: the Department of Justice and the intelligence agencies, this floor's principal opponents, comply with the warrant requirement in ordinary domestic criminal work daily, conceding the founding problem is real wherever they concede jurisdiction; the contest is confined to whether 702 queries fall inside it. The PCLOB's reports, Inspector General audits documenting query-compliance failures, and the D.C. Circuit's CDT panel opinion classifying the queries as searches further corroborate that the query-specific instance of the old problem is live. The benefiting parties' own attestations are unnecessary to the record.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__constitutional_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__constitutional_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__constitutional_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
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
 *   Epsilon is authored at 0.25, the endpoint of a rising series: the floor's bite has grown as its premises gained doctrinal traction (CDT v. Trump classifying queries as searches; the 2024 reauthorization codifying query safeguards), and the compliance burden it would impose is real but bounded, since individualized review scales through electronic submission, batching, and existing FISC infrastructure. Suppression is authored at 0.45 as a raw, unscaled structural property: the enforcement toolkit (suppression motions, FISC refusal, contempt, audit findings) has matured but remains incompletely deployed against an executive that retains partial alternatives. Theater_ratio is authored at 0.15 and, unusually, FALLS across the interval from 0.45: early FISC review of minimization procedures was largely ex parte rubber-stamping, performative in function, and the enforcement apparatus has become more genuinely adversarial (amicus advocates, mandatory error audits, codified query procedures), so the performative share of the constraint's operation shrank as its force grew. Resistance is high (0.70) because this is among the most actively contested questions in surveillance law, with sustained executive, agency, and congressional opposition. Accessibility_collapse sits at 0.45: once the floor is accepted, warrantless querying collapses as a lawful option, but the underlying intelligence need keeps alternatives alive (other legal authorities, statutory amendment, litigation), which is the rope-to-snare middle band rather than the mountain profile. The suppression_requirement series is authored deliberately because enforcement-capacity change IS the dynamic this story traces: from no adversarial process in 2008, through the post-Snowden audit regime and USA FREEDOM Act amicus provisions, to RISAA-era codified safeguards. All three series run on one shared time grid (2008, 2011, 2014, 2017, 2020, 2023, 2026) with every metric authored at every point. The claimed type (tangled_rope) is stated from structure and the metrics from description, independently: the floor possesses a genuine coordination function (a uniform, judicially supervised access procedure replacing inter-agency self-certification races, solving the legitimacy collective-action problem that keeps the program politically durable) AND asymmetric extraction (compliance costs concentrate on a single institutional payer while protection diffuses across millions of communicants), and it holds only through active enforcement, since the historical record shows agencies reverting to self-certified querying whenever the enforcement layer thins. On the receipt surface: gain_flow is authored as 'diffuse' as an affirmative checked claim — the FISC accrues docket significance, advocacy organizations accrue standing, and the defense bar accrues procedural tools, but the substantive good the constraint produces (warrant-gated access decisions, protected content) disperses across communicants and the public with no single capturing seat; the institutional rents are secondary effects, not the landing site of the burden extracted. fixing_cost is authored as 'prohibitive', anchored to REMOVAL, which is the live demand: eliminating the floor requires an Article V supermajority or the Supreme Court reversing entrenched warrant-clause doctrine, costs wildly disproportionate to what opponents expect to gain; implementation cost (FISC staffing, electronic warrant infrastructure) is a different axis, a moderate appropriation, and is not what this field records.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the beneficiary seats should compute sharply different types from identical structural data. From inside the agencies, the floor is a procedural tax on urgency: every hour of warrant latency is an hour a lead goes cold, and probable cause showings expose sources and methods to a paper record. From the communicants' seat, the same structure is the difference between a searchable life and a constitutionally gated one; they experience no burden at all, only protection. The agenda-setting seats experience administration rather than sacrifice: for the judiciary the floor is docket work, for Congress a funding and codification question. The engine computes this per-seat divergence from the role, power, and exit data; the authored claim does not adjudicate which experience is the constraint's truth.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation. executive_intelligence_agencies are declared victims with institutional power and constrained exit (they cannot exit the constitutional system itself; their alternatives are compliance, other legal authorities, or statutory amendment), placing them near the full-target end, where effective extraction is amplified. us_person_communicants are declared beneficiaries with trapped exit (incidental collection cannot be opted out of), placing them near the full-beneficiary end, where the computation damps or inverts extraction into subsidy; notably, their trap amplifies PROTECTION rather than burden because they sit on the subsidized side. civil_liberties_organizations and criminal_defense_bar are beneficiaries with mobile exit (broad issue portfolios, transferable procedural tools), sitting nearest the beneficiary end. The agenda-setting seats (article_iii_judiciary, fisa_court_judges, congress_intelligence_committees) administer without capturing, deriving mid-to-low directionality. foreign_surveillance_targets carry no beneficiary/victim declaration because the floor, as framed, neither burdens nor subsidizes them; their directionality reverts to the canonical fallback, and that silence is itself structural: the reading regulates only what happens after their communications reach U.S. analysts. No directionality_overrides are authored: the derivation from role plus exit produces the correct ordering, and the override mechanism keys on power_atom, so any override would clobber the three institutional seats that legitimately occupy different positions at the same power level.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live, so mandatrophy is not resolved and no sunset semantics apply: a constitutional floor is definitionally non-transitional, which structurally excludes the scaffold category despite the transition-like implementation period (building warrant infrastructure at query scale) that adoption would require. The classification earns its keep against mislabeling in both directions. Opponents' framing paints the floor as pure bureaucratic friction, a snare-shaped story in which coordination talk covers obstruction; that framing ignores the genuine coordination function, the uniform legitimacy procedure that replaces self-certification races and is the stated reason program defenders themselves cite judicial authorization as insurance for the program's durability. Proponents' framing paints the floor as costless idealism, a rope-shaped story; that framing ignores the real, concentrated compliance burden on a single institutional payer and the enforcement dependence the historical record demonstrates. The tangled_rope structure records both halves: someone is coordinated (executive access runs through a common judicial gate) and someone pays (the executive bears the compliance costs) through the same structure, which holds only because courts and auditors actively apply it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_structural_delta,
    'How would each sibling reading of the fisa_702_statutory_text kernel restructure this constraint''s beneficiary/victim geometry if adopted?',
    'Doctrinal resolution: a Supreme Court merits ruling on 702 queries, or statutory codification of a query-stage warrant requirement, would collapse the three-reading contest into one operative constraint; until then the readings persist as separate constraint files.',
    'Adoption of the incidental_collection_reading dissolves this constraint''s payer seat entirely (no compliance burden arises, victims list empties); adoption of the foreign_target_strict_reading shrinks this constraint to a residual access rule layered on top of collection-stage minimization, reducing its scope and bite.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer structure: this file instantiates one reading (constitutional_floor_reading) of the fisa_702_statutory_text kernel; siblings instantiate different victim/benefit geometries over the same standing arrangement.').

omega_variable(
    third_party_doctrine_reach,
    'Does Fourth Amendment doctrine reach communications content U.S. persons convey to email providers and carriers, given the third-party doctrine line (Miller, Smith), or does the warrant floor fail at the threshold?',
    'Carpenter''s narrowing of the third-party doctrine signals retreat for sensitive content; a merits ruling on the search status of content queries against stored provider-held communications would settle the threshold question.',
    'If third-party doctrine bars the reading, the constraint never binds anyone and effective extraction collapses toward zero regardless of authored epsilon; if Carpenter''s logic extends to content, the floor attaches squarely and the payer seat''s burden is fully live.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_party_doctrine_reach, conceptual, 'Threshold doctrinal risk to whether the reading applies at all.').

omega_variable(
    special_needs_exception_scope,
    'Even granting that 702 queries are Fourth Amendment searches, does the foreign-intelligence special-needs rationale (as applied in CDT v. Trump) excuse the warrant requirement categorically?',
    'Supreme Court engagement with reasonableness balancing for database queries of U.S. person content, informed by PCLOB and Inspector General data on query volumes, utility, and compliance failures.',
    'If the exception swallows the floor, the searches classification stands but the warrant requirement drops out: the constraint''s bite and effective extraction collapse while its classification vocabulary survives intact, splitting the story''s two halves.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(special_needs_exception_scope, conceptual, 'Reasonableness-exception risk sitting between search-classification and remedy.').

omega_variable(
    individualized_review_cost_magnitude,
    'What is the true operational cost of individualized probable cause review at 702 query scale: is the executive''s administrative-impossibility claim accurate, or are reported query volumes padded and batch electronic submission viable?',
    'Audited query-volume data (PCLOB, DOJ National Security Division IG), pilot programs with warrant-backed query tiers, and engineering estimates for electronic warrant workflows at the FISA Court.',
    'A low true cost supports treating the burden as ordinary coordination overhead and pushes the computed classification toward the rope side; a genuinely prohibitive cost would force program redesign and lend the constraint a transitional character the current story does not claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(individualized_review_cost_magnitude, empirical, 'Magnitude of the compliance burden the floor actually imposes on executive operations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__constitutional_floor_reading, 2008, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t2008, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2008, 0.45).
narrative_ontology:measurement(fisa_tr_t2011, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2011, 0.41).
narrative_ontology:measurement(fisa_tr_t2014, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2014, 0.36).
narrative_ontology:measurement(fisa_tr_t2017, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2017, 0.31).
narrative_ontology:measurement(fisa_tr_t2020, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2020, 0.26).
narrative_ontology:measurement(fisa_tr_t2023, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2023, 0.19).
narrative_ontology:measurement(fisa_tr_t2026, fisa_702_statutory_text__constitutional_floor_reading, theater_ratio, 2026, 0.15).

% Extraction over time
narrative_ontology:measurement(fisa_be_t2008, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2008, 0.08).
narrative_ontology:measurement(fisa_be_t2011, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2011, 0.11).
narrative_ontology:measurement(fisa_be_t2014, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2014, 0.15).
narrative_ontology:measurement(fisa_be_t2017, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2017, 0.19).
narrative_ontology:measurement(fisa_be_t2020, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2020, 0.22).
narrative_ontology:measurement(fisa_be_t2023, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2023, 0.24).
narrative_ontology:measurement(fisa_be_t2026, fisa_702_statutory_text__constitutional_floor_reading, base_extractiveness, 2026, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t2008, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2008, 0.12).
narrative_ontology:measurement(fisa_su_t2011, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2011, 0.17).
narrative_ontology:measurement(fisa_su_t2014, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2014, 0.26).
narrative_ontology:measurement(fisa_su_t2017, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2017, 0.33).
narrative_ontology:measurement(fisa_su_t2020, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2020, 0.39).
narrative_ontology:measurement(fisa_su_t2023, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2023, 0.43).
narrative_ontology:measurement(fisa_su_t2026, fisa_702_statutory_text__constitutional_floor_reading, suppression_requirement, 2026, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__constitutional_floor_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__incidental_collection_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__constitutional_floor_reading, fisa_702_statutory_text__foreign_target_strict_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the natural-language label 'Section 702 safeguards for U.S. persons' conflates three structurally distinct claims, modeled as three linked stories. This file (constitutional_floor_reading) authors the query-stage warrant floor, with epsilon approximately 0.25 pricing the compliance burden the floor imposes on executive speed and secrecy preferences. fisa_702_statutory_text__foreign_target_strict_reading authors collection-stage targeting limits and minimization duties. fisa_702_statutory_text__incidental_collection_reading authors the permission structure for warrantless querying, pricing the standing arrangement near zero by its own lights. Each story carries its own epsilon, beneficiaries, and victims; the family link records that upstream doctrinal holdings (queries-are-searches) feed the downstream contests over remedy and statute, and that the incidental reading is cited as settled practice in arguments against this one.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
