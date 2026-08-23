% ============================================================================
% CONSTRAINT STORY: fisa_702_statutory_text__incidental_collection_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: fisa_702_statutory_text__incidental_collection_reading
 *   human_readable: FISA Section 702 Incidental-Collection Reading: Retention and Warrantless Query of U.S.-Person Communications
 *   domain: constitutional_law/national_security
 *
 * SUMMARY:
 *   Under the incidental-collection reading of the Section 702 text,
 *   communications acquired through targeting of non-U.S. persons abroad may
 *   be retained indefinitely and queried by U.S.-person identifier without an
 *   individualized court order whenever a foreign-intelligence purpose
 *   justifies the query; administrative minimization procedures, written and
 *   applied by the collecting agencies themselves, stand in place of judicial
 *   authorization. The practical consequence is that U.S. persons enter the
 *   searched population through two doors — incidental sweep-in at collection
 *   and direct identifier query during domestic investigations — with no
 *   notice, no consent, and no exit. This file instantiates ONE reading of
 *   the contested statutory kernel; the sibling readings
 *   (foreign_target_strict_reading, constitutional_floor_reading) are
 *   separate constraints in separate files, linked through the network
 *   section. Per the epsilon-referent rule for kernel readings, the authored
 *   epsilon describes the standing incidental-collection arrangement as this
 *   reading assesses it — substantial but bounded intrusion, acknowledged
 *   even by the reading that endorses the arrangement — not the arrangement a
 *   sibling reading would put in its place. The claim and the metrics are
 *   independent authored facts: the claimed type states what I believe
 *   structurally true of this arrangement; the metrics state what I believe
 *   descriptively true of its operation; the engine computes per-seat
 *   classifications from the structural data and owns any divergence.
 *
 * KEY AGENTS:
 *   - signals_intelligence_agencies: agenda-setting collector ([institutional]/[arbitrage]) — writes the minimization procedures, holds the repositories, drafts each reauthorization case
 *   - federal_bureau_of_investigation: primary domestic beneficiary ([institutional]/[mobile]) — queries the repository by U.S.-person identifier without individualized court orders
 *   - executive_branch_national_security_establishment: institutional beneficiary ([institutional]/[arbitrage]) — defends the reading, certifies compliance, controls classification
 *   - congressional_oversight_committees: co-agenda-setter ([institutional]/[arbitrage]) — renews the authority; warrant-requirement amendments repeatedly fail narrowly
 *   - us_person_communicants: primary target ([powerless]/[trapped]) — swept in incidentally, unable to know, decline, or contest
 *   - backdoor_search_targets: primary target ([powerless]/[trapped]) — searched directly by identifier in domestic investigations
 *   - privileged_professional_communicants: concentrated target ([moderate]/[trapped]) — journalists, counsel, clergy whose obligatory confiding traffic is disproportionately collected
 *   - foreign_intelligence_targets: nominal target ([powerless]/[trapped]) — outside the constitutional order entirely
 *   - foreign_intelligence_surveillance_court: analytical observer ([institutional]/[analytical]) — ex parte procedure review, delayed declassification
 *   - civil_liberties_litigators: excluded challenger ([organized]/[analytical]) — no seat in the approval process, reaches the court only via amicus
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fisa_702_statutory_text__incidental_collection_reading, 0.45).
domain_priors:suppression_score(fisa_702_statutory_text__incidental_collection_reading, 0.62).
domain_priors:theater_ratio(fisa_702_statutory_text__incidental_collection_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(fisa_702_statutory_text__incidental_collection_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fisa_702_statutory_text__incidental_collection_reading, tangled_rope).
narrative_ontology:human_readable(fisa_702_statutory_text__incidental_collection_reading, "FISA Section 702 Incidental-Collection Reading: Retention and Warrantless Query of U.S.-Person Communications").
narrative_ontology:topic_domain(fisa_702_statutory_text__incidental_collection_reading, "constitutional_law/national_security").

domain_priors:requires_active_enforcement(fisa_702_statutory_text__incidental_collection_reading).
narrative_ontology:has_sunset_clause(fisa_702_statutory_text__incidental_collection_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fisa_702_statutory_text__incidental_collection_reading, '2fc9ed18-de18-46b4-8142-acb72f606150').
narrative_ontology:cs_kernel_codification('2fc9ed18-de18-46b4-8142-acb72f606150', fixed_text).
narrative_ontology:cs_authority_grounding('2fc9ed18-de18-46b4-8142-acb72f606150', extraction).
narrative_ontology:cs_interpretation_layer_present('2fc9ed18-de18-46b4-8142-acb72f606150').
narrative_ontology:cs_reading_relation('2fc9ed18-de18-46b4-8142-acb72f606150', fisa_702_statutory_text__foreign_target_strict_reading, coexists_with).
narrative_ontology:cs_reading_relation('2fc9ed18-de18-46b4-8142-acb72f606150', fisa_702_statutory_text__constitutional_floor_reading, forecloses).
narrative_ontology:cs_axiom('2fc9ed18-de18-46b4-8142-acb72f606150', foundational, foreign_intelligence_purpose_displaces_warrant_requirement).
narrative_ontology:cs_axiom_status(foreign_intelligence_purpose_displaces_warrant_requirement, holdable).
narrative_ontology:cs_axiom_grounding('2fc9ed18-de18-46b4-8142-acb72f606150', foreign_intelligence_purpose_displaces_warrant_requirement, conventional).
narrative_ontology:cs_axiom('2fc9ed18-de18-46b4-8142-acb72f606150', foundational, administrative_minimization_substitutes_for_judicial_authorization).
narrative_ontology:cs_axiom_status(administrative_minimization_substitutes_for_judicial_authorization, holdable).
narrative_ontology:cs_axiom_grounding('2fc9ed18-de18-46b4-8142-acb72f606150', administrative_minimization_substitutes_for_judicial_authorization, instrumental).
narrative_ontology:cs_reference_frame('2fc9ed18-de18-46b4-8142-acb72f606150', foreign_targeting_minimization_regime).
narrative_ontology:cs_drift_state('2fc9ed18-de18-46b4-8142-acb72f606150', post_risaa_reauthorization, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('2fc9ed18-de18-46b4-8142-acb72f606150', '').
narrative_ontology:cs_kernel_id(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, signals_intelligence_agencies).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, federal_bureau_of_investigation).
narrative_ontology:constraint_beneficiary(fisa_702_statutory_text__incidental_collection_reading, executive_branch_national_security_establishment).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, us_person_communicants).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, backdoor_search_targets).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, privileged_professional_communicants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(fisa_702_statutory_text__incidental_collection_reading, foreign_intelligence_targets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collects communications of non-U.S. persons reasonably believed to be located abroad under the statutory authority, writes and applies the minimization procedures that determine what incidentally collected U.S.-person content is kept, and operates the repositories that analysts query. As the authority approaches each expiration date it drafts the reauthorization case and supplies the intelligence-value examples that sustain it. Its alternatives are rich: it can shift collection onto other legal authorities, adjust targeting, or reshape its own procedures; abandoning the arrangement is not something it needs to contemplate.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, signals_intelligence_agencies, agenda_setter,
    institutional, generational, arbitrage, global).

% Queries the collected repository using U.S.-person identifiers tied to its own counterintelligence, counterterrorism, and ordinary criminal investigations, without obtaining individualized court orders for those searches. Warrant-based paths remain open to it but are slower and narrower; the repository offers immediate, broad access. It reports query statistics to oversight bodies and adjusts its procedures when reviewers find violations.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, federal_bureau_of_investigation, beneficiary,
    institutional, biographical, mobile, national).

% The departments and offices that defend the legal reading, certify minimization compliance, classify the program's mechanics, and negotiate each reauthorization. It bears essentially none of the privacy cost and collects the institutional benefit of an access path unencumbered by individualized court orders; its exit is trivial because it controls the procedures themselves.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, executive_branch_national_security_establishment, beneficiary,
    institutional, generational, arbitrage, global).

% Writes and periodically renews the statutory authority and runs oversight hearings fed by classified briefings. Members who favor narrowing the query rules have repeatedly introduced warrant-requirement amendments that fail by narrow margins; members who favor renewal control the scheduling. The committees' institutional position depends on the authority existing to be overseen and renewed.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, congressional_oversight_committees, agenda_setter,
    institutional, generational, arbitrage, national).

% Anyone in the United States who communicates with a person or account overseas that meets the targeting criteria. Their messages enter the collected repositories without their knowledge, consent, or any individualized determination about them; they cannot find out whether they were collected, cannot decline, and have no procedural path to contest retention. Their only partial shield is end-to-end encryption, which the collection architecture increasingly routes around or renders moot at the endpoints.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, us_person_communicants, payer,
    powerless, biographical, trapped, global).

% People whose own identifiers — name, email address, phone number — are entered into the repository during domestic investigations. They are searched directly rather than incidentally, without a warrant and usually without ever learning the search occurred; disclosure typically arrives only if material surfaces in a criminal case, sometimes years afterward.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, backdoor_search_targets, payer,
    powerless, biographical, trapped, national).

% Journalists, defense counsel, clergy, and physicians whose work requires confiding exchanges with sources, clients, and contacts abroad. Their professional obligations make avoidance impossible — declining the communication ends the service — and the sensitivity of their traffic makes them disproportionately represented among incidentally collected content. Institutions exist to litigate on their behalf, but individual members cannot protect their own files.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, privileged_professional_communicants, payer,
    moderate, biographical, trapped, global).

% Non-U.S. persons abroad whose communications the authority is designed to acquire. They stand wholly outside the constitutional protections invoked on behalf of U.S. persons, have no forum in which to object, and bear the direct weight of the collection the arrangement exists to perform.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, foreign_intelligence_targets, payer,
    powerless, biographical, trapped, global).

% Reviews targeting and minimization procedures and hears agency reports on query compliance, meeting predominantly ex parte with classified materials. It approves, modifies, and occasionally rebukes the procedures, and its opinions — mostly declassified long after the decisions — are the nearest thing to adversarial testing the arrangement receives.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, foreign_intelligence_surveillance_court, observer,
    institutional, generational, analytical, national).

% Advocacy organizations and defense-bar networks that challenge the query practices in public litigation and press warrant-requirement amendments. They hold no seat in the procedure-approval process, reach the reviewing court only through rare amicus appointments, and learn program details chiefly through leaks and declassification.
narrative_ontology:constraint_stakeholder(fisa_702_statutory_text__incidental_collection_reading, civil_liberties_litigators, excluded,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fisa_702_statutory_text__incidental_collection_reading, signals_intelligence_agencies).
narrative_ontology:fixing_cost_class(fisa_702_statutory_text__incidental_collection_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates signals intelligence on foreign threats — terrorism, proliferation, cyber intrusion — by acquiring communications of non-U.S. persons abroad at scale, solving a collection problem that individualized, per-target court orders handle too slowly and too narrowly for distributed foreign communications.
% TRANSFER_FUNCTION: Moves communicative privacy — access to the content and metadata of communications involving U.S. persons — from U.S. persons, without notice, consent, or warrant, to intelligence and domestic-investigation agencies, under administrative minimization procedures rather than judicial authorization.
% ABSENT_VOICES: The queried U.S. persons themselves: most never learn their communications were searched and so cannot object anywhere. Civil-liberties litigators hold no seat in the procedure-approval process and reach the reviewing court only through occasional amicus appointments. Defense counsel frequently cannot learn whether case evidence originated in the repository until late disclosure, if at all.
% DISAPPEARANCE_RATIONALE: If the retention-and-query authority vanished overnight, agencies would need individualized court orders or far stricter handling for U.S.-person data already collected; domestic investigations would lose a major evidentiary shortcut and reorganize around slower warrant-based paths; foreign-intelligence collection would continue but with materially higher handling costs and smaller usable corpora. The arrangements of every seated party depend on the authority continuing.
% FOUNDING_PROBLEM: The pre-2008 framework required an individualized court order for each acquisition, which proved ill-suited to intercepting distributed foreign communications — email, voice-over-IP, cloud-stored content — transiting United States infrastructure; the 2008 amendments were built to authorize acquisition directed at foreign targets without a per-target order.
% FOUNDING_PROBLEM_CORROBORATION: Partially corroborated from outside the benefiting parties: the Privacy and Civil Liberties Oversight Board's independent review attests both the reality of the foreign-collection function and its drift toward domestic investigative use; declassified reviewing-court opinions acknowledge the foreign-targeting purpose while documenting query violations; the pre-2008 collection-gap problem is documented in prior independent commission records. No fully disinterested body attests that the founding problem persists in its original form — congressional privacy caucuses and civil-liberties litigators dispute that the original gap justifies the current query practice.
narrative_ontology:disappearance_verdict(fisa_702_statutory_text__incidental_collection_reading, world_rearranges).
narrative_ontology:founding_problem_status(fisa_702_statutory_text__incidental_collection_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fisa_702_statutory_text__incidental_collection_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
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
 *   Extraction is authored at 0.45: the arrangement grants warrantless access to the content of rights-holders' communications — a substantial taking — but the taking is bounded by the foreign-targeting anchor, by minimization procedures that have demonstrably tightened after documented violations, and by periodic reauthorization scrutiny. Suppression is authored at 0.62 and is structural, not violent: classification, absence of notice, ex parte review, and the impossibility of opting out of communicating with the world. Suppression is a raw structural property and is deliberately NOT scaled by power or scope — only extractiveness is scaled, by directionality and spatial scope, in the engine's computation. Theater ratio 0.38: oversight activity is partly functional (it produced real procedural corrections after 2021) but a growing share is compliance performance — statistics reports, certification letters, anniversary-value anecdotes — that ratifies rather than tests. Accessibility collapse 0.58: the alternatives are legally coherent (the strict sibling reading demonstrates that aggressive minimization is statutorily implementable; warrant-based querying demonstrably works) but are politically foreclosed — which is why collapse is substantial yet short of the near-total collapse of a natural limit. Resistance 0.6: sustained litigation, narrowly failing warrant amendments across multiple reauthorization cycles, and recurring legislative revolt attempts. The statute's literal sunset clause is declared because it exists, but it operates as a reauthorization ratchet rather than a transition plan — each expiry has been the occasion for renewal, occasionally with margin-narrowing safeguards, never for wind-down. The measurement series run on one shared time grid (t=0 maps to 2008 enactment; t=4 to the 2012 renewal; t=8 to 2016 mid-cycle query growth; t=12 to 2020 peak non-compliance; t=14 to the 2022 violation findings and imposed reforms; t=16 to the 2024 reauthorization), with every tracked metric authored at every point; the extractiveness series shows accumulation followed by partial correction, and the end-state values equal the base_properties scalars.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats should compute divergent types from identical structural data. From the collecting agencies' position the arrangement is a lawful authority they administer, with costs borne by people who are not in the room; from the trapped U.S.-person seats the same structure operates as unconsented, unnegotiable access to their private communications — an experience closer to pure extraction with a coordination alibi. The reviewing court sits between: it sees the procedures' paper rigor and the violations' paper frequency. The engine computes this divergence per seat; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: the collecting agencies, the domestic-investigation bureau, and the executive establishment all sit near the beneficiary end (low d, damped or inverted effective extraction) — each controls its own alternatives, and the establishment controls the procedures themselves. Victim declarations drive the opposite pole: U.S.-person communicants, backdoor-search targets, and privileged professional communicants are trapped rights-holders with no notice and no exit, sitting near the full-target end, so effective extraction is amplified for them; larger spatial scope (global) further amplifies verification difficulty. Foreign-intelligence targets are the designed objects of the collection and likewise derive high d. No directionality overrides are authored: the structural derivation from declared roles, power, and exit options captures every seat accurately, and the override mechanism keys on the power atom rather than the agent — with five institutional seats in this story holding genuinely different relationships, a power-atom-level override would misapply across all of them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — efficient acquisition of distributed foreign communications — is genuinely partly live, which is exactly why this must not be flattened into pure extraction: doing so would erase the real collective-security coordination that anchors the arrangement and misread a working intelligence capability as mere cover. Equally, the arrangement must not be flattened into pure coordination: the domestic-query function has grown far beyond the foreign-targeting rationale, transferring communicative privacy from non-consenting rights-holders to investigating agencies through the same structure that performs the coordination. The hybrid classification holds both facts in tension. On the genealogy interview, the founding problem's status is contested and the disappearance verdict is world_rearranges — the mismatch consumer therefore reads no dead-mandate-plus-dependence flag, but the temporal series carry the sharper signal: extraction accumulated monotonically for twelve years before disclosure forced partial correction, the classic signature of a coordination function accreting an extraction rider faster than its oversight can strip it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the kernel fisa_702_statutory_text (reading: incidental_collection_reading). Which reading of the statutory text governs actual collection and query practice — and how would the victim set and epsilon change under the sibling readings?',
    'Supreme Court adjudication of a repository-query case, statutory amendment imposing a warrant requirement for U.S.-person queries, or sustained reviewing-court procedural rulings; each would shift the operative reading and re-partition the victim set.',
    'Under foreign_target_strict_reading the U.S.-person victim set contracts to residual incidental exposure and epsilon falls toward coordination cost; under constitutional_floor_reading the identical conduct is re-described as warrantless search, epsilon rises sharply and the classification migrates toward pure extraction. The authored epsilon of 0.45 is valid only for this reading; it is not a property of the statutory text as such.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed identity: sibling readings of the same statutory text instantiate different constraints with different victim sets and epsilon values.').

omega_variable(
    query_necessity_separability,
    'Is warrantless querying of incidentally collected U.S.-person communications necessary to the foreign-intelligence mission, or is it separable — could agencies obtain individualized court orders for U.S.-person queries at modest mission cost?',
    'Operational comparison of mission output under periods or regimes with enhanced query procedures (the post-2021 imposed query procedures, the codified audit-trail requirements) against baseline periods of unrestricted querying.',
    'If separable, the queried access is extraction riding on genuine foreign-targeting coordination and effective extraction rises above the authored value; if inseparable, part of the authored epsilon is the price of the coordination itself and the hybrid-coordination reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(query_necessity_separability, empirical, 'Whether the extraction component is structurally separable from the coordination component.').

omega_variable(
    minimization_enforcement_gap,
    'Do the minimization and oversight procedures constrain retention and querying as the reading claims, or is compliance substantially self-certified?',
    'Independent oversight audits, declassified reviewing-court compliance opinions, and inspector-general reports comparing declared procedure against observed query behavior — including the documented 2020-2023 findings of widespread non-compliant Federal Bureau of Investigation queries and the post-reform violation-rate data.',
    'A wide gap between declared procedure and observed practice raises effective extraction above the authored 0.45 and pushes the theater ratio upward; demonstrated conformity supports the reading''s own assessment of the arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(minimization_enforcement_gap, empirical, 'Whether administrative safeguards bind in practice or operate mainly on paper.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fisa_702_statutory_text__incidental_collection_reading, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fisa_tr_t0, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(fisa_tr_t4, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 4, 0.25).
narrative_ontology:measurement(fisa_tr_t8, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 8, 0.3).
narrative_ontology:measurement(fisa_tr_t12, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 12, 0.36).
narrative_ontology:measurement(fisa_tr_t14, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 14, 0.38).
narrative_ontology:measurement(fisa_tr_t16, fisa_702_statutory_text__incidental_collection_reading, theater_ratio, 16, 0.38).

% Extraction over time
narrative_ontology:measurement(fisa_be_t0, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(fisa_be_t4, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 4, 0.33).
narrative_ontology:measurement(fisa_be_t8, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(fisa_be_t12, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 12, 0.44).
narrative_ontology:measurement(fisa_be_t14, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 14, 0.46).
narrative_ontology:measurement(fisa_be_t16, fisa_702_statutory_text__incidental_collection_reading, base_extractiveness, 16, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(fisa_su_t0, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(fisa_su_t4, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 4, 0.52).
narrative_ontology:measurement(fisa_su_t8, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(fisa_su_t12, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement(fisa_su_t14, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 14, 0.61).
narrative_ontology:measurement(fisa_su_t16, fisa_702_statutory_text__incidental_collection_reading, suppression_requirement, 16, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fisa_702_statutory_text__incidental_collection_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text__foreign_target_strict_reading).
narrative_ontology:affects_constraint(fisa_702_statutory_text__incidental_collection_reading, fisa_702_statutory_text__constitutional_floor_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'Section 702.' The single statutory text is a contested kernel read three ways, each reading instantiating a distinct constraint with its own stable epsilon and victim set: this file (incidental_collection_reading — retention and warrantless foreign-purpose query of incidentally collected U.S.-person communications, epsilon approximately 0.45, U.S. persons in the victim set via backdoor searches); foreign_target_strict_reading (incidental U.S.-person data minimized to practical inaccessibility for domestic use — victim set contracts toward foreign targets only, epsilon falls toward coordination cost); and constitutional_floor_reading (any U.S.-person content query is a search requiring a probable-cause warrant regardless of statutory construction — the same conduct re-described as warrantless search, epsilon rises sharply). The epsilon values differ because the readings disagree about what the arrangement IS, not because one observable was swapped for another; per the epsilon-invariance principle they are separate stories linked here rather than one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
