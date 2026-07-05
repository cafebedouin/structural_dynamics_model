% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__jurisdictional_capture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__jurisdictional_capture_reading, []).

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
 *   constraint_id: nsl_legal_text__jurisdictional_capture_reading
 *   human_readable: National Security Law as Mainland Legal-System Transplantation Vehicle
 *   domain: constitutional_law/political_sociology/international_relations
 *
 * SUMMARY:
 *   This story instantiates the jurisdictional_capture_reading of the
 *   nsl_legal_text kernel: the National Security Law examined specifically as
 *   a mechanism for transplanting mainland legal-system features
 *   (interpretive supremacy of the NPCSC, non-jury security tribunals,
 *   presumptive detention, extraterritorial mainland-staffed enforcement
 *   immune from HK process) into a jurisdiction whose Basic Law guarantees a
 *   separate common law system until 2047. This reading is narrower than
 *   either the democratic-enclosure reading (which centers dissent
 *   criminalization) or the sovereignty-restoration reading (which centers
 *   the legitimacy of the security response to 2019). Here the object of
 *   analysis is institutional architecture: does the statute's operation
 *   erode HK's common law autonomy as a legal system, independent of whether
 *   the underlying security concern was genuine. The victim set here is
 *   deliberately narrow and institutional — judiciary, legal profession,
 *   litigants caught in designated cases — not the broader population of
 *   political dissidents (that is the sibling reading's victim set).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__jurisdictional_capture_reading, 0.68).
domain_priors:suppression_score(nsl_legal_text__jurisdictional_capture_reading, 0.71).
domain_priors:theater_ratio(nsl_legal_text__jurisdictional_capture_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__jurisdictional_capture_reading, tangled_rope).
narrative_ontology:human_readable(nsl_legal_text__jurisdictional_capture_reading, "National Security Law as Mainland Legal-System Transplantation Vehicle").
narrative_ontology:topic_domain(nsl_legal_text__jurisdictional_capture_reading, "constitutional_law/political_sociology/international_relations").

domain_priors:requires_active_enforcement(nsl_legal_text__jurisdictional_capture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__jurisdictional_capture_reading, 'c91120cc-8702-4c66-b90d-f4cfbb0bcf1b').
narrative_ontology:cs_kernel_codification('c91120cc-8702-4c66-b90d-f4cfbb0bcf1b', formalized).
narrative_ontology:cs_authority_grounding('c91120cc-8702-4c66-b90d-f4cfbb0bcf1b', extraction).
narrative_ontology:cs_interpretation_layer_present('c91120cc-8702-4c66-b90d-f4cfbb0bcf1b').
narrative_ontology:cs_reading_relation('c91120cc-8702-4c66-b90d-f4cfbb0bcf1b', nsl_legal_text__sovereignty_restoration_reading, influences).
narrative_ontology:cs_reading_relation('c91120cc-8702-4c66-b90d-f4cfbb0bcf1b', nsl_legal_text__democratic_enclosure_reading, coexists_with).
narrative_ontology:cs_axiom('c91120cc-8702-4c66-b90d-f4cfbb0bcf1b', foundational, common_law_interpretive_method_is_non_fungible_institutional_capital).
narrative_ontology:cs_axiom_status(common_law_interpretive_method_is_non_fungible_institutional_capital, holdable).
narrative_ontology:cs_axiom_grounding('c91120cc-8702-4c66-b90d-f4cfbb0bcf1b', common_law_interpretive_method_is_non_fungible_institutional_capital, conventional).
narrative_ontology:cs_axiom('c91120cc-8702-4c66-b90d-f4cfbb0bcf1b', foundational, npcsc_binding_interpretation_supersedes_local_judicial_finality).
narrative_ontology:cs_axiom_status(npcsc_binding_interpretation_supersedes_local_judicial_finality, holdable).
narrative_ontology:cs_axiom_grounding('c91120cc-8702-4c66-b90d-f4cfbb0bcf1b', npcsc_binding_interpretation_supersedes_local_judicial_finality, conventional).
narrative_ontology:cs_reference_frame('c91120cc-8702-4c66-b90d-f4cfbb0bcf1b', basic_law_common_law_autonomy_guarantee).
narrative_ontology:cs_drift_state('c91120cc-8702-4c66-b90d-f4cfbb0bcf1b', post_2020_nsl_enactment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c91120cc-8702-4c66-b90d-f4cfbb0bcf1b', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus).
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, office_for_safeguarding_national_security).
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, central_government_legal_officials).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_judiciary).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_legal_profession).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, common_law_litigants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates the Office for Safeguarding National Security in Hong Kong under NSL Article 60, which sits outside HK judicial jurisdiction. Can designate cases for mainland trial under Article 55, transferring proceedings from common law courts to mainland procedure entirely. Sets the substantive definitions (secession, subversion, collusion) that HK courts must apply but did not draft and cannot meaningfully narrow through interpretation.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus, beneficiary).

% The mainland-staffed body operating in Hong Kong with immunity from HK legal process (Article 60). Selects which cases proceed under NSL rather than ordinary HK criminal law, effectively choosing which forum and which procedural tradition governs a given prosecution. Its personnel are not subject to HK judicial oversight.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, office_for_safeguarding_national_security, agenda_setter,
    institutional, generational, analytical, national).

% Gain a standing instrument by which mainland statutory interpretation practice (the NPCSC's power to issue binding interpretations under Article 65) can override or preempt HK common law interpretive method. Each interpretive intervention establishes precedent for treating HK's separate legal system as subordinate rather than autonomous, which strengthens the long-run integration agenda without requiring formal Basic Law amendment.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, central_government_legal_officials, beneficiary,
    institutional, civilizational, analytical, national).

% HK judges, including at the Court of Final Appeal, must apply NSL provisions drafted in mainland legislative style (broad, purpose-oriented, security-first) using common law interpretive method built for narrow construction and precedent. Where interpretation is unresolved, the NPCSC can issue a binding interpretation that judges must follow, which no HK court can review or reverse. Judges who resist face reassignment pressure on the certified-judges list (Article 44) or public mainland criticism; their sentencing discretion in designated cases is statutorily narrowed. Exit is limited to resignation or early retirement — the judiciary as an institution cannot leave the jurisdiction it sits within.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_judiciary, payer,
    moderate, biographical, constrained, regional).

% Barristers and solicitors built professional identity and practice around common law adversarial procedure, precedent-based advocacy, and jury trial norms. NSL cases can be tried without jury before a panel of certified judges, and bail is presumptively denied under Article 42, both departures from ordinary HK criminal procedure. Practitioners who take on NSL defense work risk professional and reputational consequences; some senior counsel have left the jurisdiction. Remaining in practice means operating inside a bifurcated system where a growing category of cases follows an imported procedural logic they did not train in and cannot contest through normal appellate argument.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_legal_profession, payer,
    moderate, biographical, constrained, regional).

% Defendants and civil parties whose disputes happen to touch on matters classifiable as national security find their case pulled out of ordinary procedural protections — bail, jury, open evidentiary rules — into a track shaped by mainland practice. They have no forum-choice; the OSNSS or prosecution decides whether a case is designated. They bear the direct cost of procedural transplantation with no ability to opt back into common law process.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, common_law_litigants, payer,
    powerless, immediate, trapped, local).

% Relies on the reputational value of Hong Kong's separate, predictable common law system for contract enforcement and dispute resolution. Has no seat in NSL implementation decisions and no forum to object within HK's own institutions, though it can and does exit by relocating regional headquarters or arbitration clauses to Singapore or elsewhere — a market-level response rather than a legal one.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, international_business_community, excluded,
    organized, biographical, mobile, global).

% Study the NSL's drafting style, its interpretive mechanisms, and the pattern of NPCSC interventions as a case study in how a civil-law/socialist-legalist system can be layered onto a common law jurisdiction without formal system replacement. Produce comparative analysis independent of both Beijing's and HK democratic opposition's framings.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, comparative_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nsl_legal_text__jurisdictional_capture_reading, diffuse).
narrative_ontology:fixing_cost_class(nsl_legal_text__jurisdictional_capture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, centrally authored statute defining national-security offenses in Hong Kong, replacing the absence of any HK-enacted Article 23 legislation and giving prosecutors and mainland authorities a common instrument for handling security-classified cases across the mainland-HK boundary.
% TRANSFER_FUNCTION: Moves interpretive authority over an expanding category of legal disputes from HK common law courts and the HK legal profession to mainland-controlled bodies (NPCSC, OSNSS), and moves procedural norms (jury absence, bail presumption, judge certification) from common law form to mainland-influenced form, without formally repealing the Basic Law's common law guarantee.
% ABSENT_VOICES: The Hong Kong Bar Association and Law Society raised procedural objections during and after enactment but were not consulted in drafting, which occurred in Beijing without HK legislative process. International legal observers and the departing pool of overseas non-permanent CFA judges have flagged the interpretive-authority transfer but hold no formal standing in the mainland legislative process that produced and periodically reinterprets the statute.
% DISAPPEARANCE_RATIONALE: If the NSL and its transplantation mechanisms vanished, HK courts would revert to exclusive common law procedure for all cases, NPCSC interpretive intervention in HK matters would lose its statutory hook, the OSNSS would lose jurisdiction, and the pool of practitioners and CFA judges who left over rule-of-law concerns would face a materially different calculus about returning or staying.
% FOUNDING_PROBLEM: The stated founding problem was the absence of Article 23 national security legislation in Hong Kong, a gap the Basic Law had left unfilled for over two decades, combined with the 2019 protests and unrest which the central government treated as an active security emergency requiring an instrument HK's own legislature had failed to produce.
% FOUNDING_PROBLEM_CORROBORATION: Central government officials attest the security gap remains live and the NSL closed it appropriately. Independent comparative legal scholars and the HK Bar Association attest that whatever security gap existed, the mechanism chosen imports mainland interpretive and procedural authority into HK's separate legal system in a way that exceeds what a security statute alone would require — the transplantation function operates independently of, and outlasts, the emergency that is said to justify it.
narrative_ontology:disappearance_verdict(nsl_legal_text__jurisdictional_capture_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__jurisdictional_capture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__jurisdictional_capture_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nsl_legal_text__jurisdictional_capture_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__jurisdictional_capture_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__jurisdictional_capture_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__jurisdictional_capture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__jurisdictional_capture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored moderate-high (0.68 by interval end) because the transplantation captures a scarce, non-fungible institutional good — common law interpretive autonomy — that cannot be restored piecemeal once NPCSC interpretive precedent accumulates. Suppression (0.71) reflects the structural fact that HK courts cannot review or reverse NPCSC interpretations and cannot decline OSNSS case designation; this is a raw structural property, not scaled by scope. Theater ratio rises to 0.42 because a growing share of NSL apparatus activity (certified-judges lists, procedural announcements) performs continuity with common law form while substantive interpretive authority has already migrated upstream — genuine coordination function (closing the Article 23 gap) persists alongside this drift, which is what makes tangled_rope rather than snare the structurally correct claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Mainland security and legal officials sit near the beneficiary end: they gain a durable instrument for extending interpretive reach into HK without amending the Basic Law, at negligible direct cost to themselves. HK judiciary and legal profession sit near the target end: constrained exit (a judiciary cannot relocate; a bar cannot practice in a different jurisdiction's common law system while staying in HK), non-fungible professional identity investment, and no institutional route to contest the interpretive transfer. Common law litigants in designated cases are trapped — individual, immediate, powerless, with the highest directionality toward extraction because they bear the procedural cost with zero forum choice.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (the Article 23 legislative gap) has a contested status precisely because the transplantation mechanism has outlived and outgrown what closing that gap required. A security statute could close the gap without transferring interpretive supremacy to a body outside HK judicial review; the NSL's continued operation of that transfer after the 2019 emergency subsided is the mandatrophy signature — coordination function (security legislation) persists as cover for an extraction function (interpretive capture) that was never sunset and shows no declared mechanism for narrowing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_jurisdictional,
    'Is the jurisdictional-capture framing (institutional architecture erosion) the primary structural fact about the NSL, or is it downstream of the sovereignty-restoration framing (legitimate security response) or the democratic-enclosure framing (dissent suppression)?',
    'Track whether NPCSC interpretive interventions and OSNSS case designations concentrate on genuine cross-border security threats (supporting sovereignty-restoration as primary) or on political speech/association cases (supporting democratic-enclosure as primary) or on institutionally significant precedent-setting regardless of case content (supporting jurisdictional-capture as primary). Case-designation pattern data over the next decade would discriminate between readings.',
    'If designations concentrate on precedent-setting institutional cases rather than either pure security threats or pure dissent cases, the jurisdictional-capture reading''s claim that transplantation is the primary function (not merely a side effect of security enforcement or dissent suppression) is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_jurisdictional, conceptual, 'Which kernel reading captures the NSL''s primary structural function is itself contested and not resolvable from the text alone.').

omega_variable(
    genuine_security_gap_vs_pretext,
    'Was the Article 23 legislative gap a genuine coordination problem requiring central intervention, or was the gap itself a pretext-enabling condition that made NSL''s interpretive-transfer mechanism politically viable?',
    'Compare the statute''s actual provisions against what a security statute drafted by HK''s own legislature under Article 23 would plausibly have contained (e.g., prior HK government consultation drafts, comparable common law jurisdiction national security statutes). A large divergence in interpretive-authority allocation, beyond what security substance requires, supports the pretext reading.',
    'If the gap was genuine and the interpretive-transfer provisions are what any security statute in a unitary state would require, this reading''s extraction estimate should be revised downward toward rope/scaffold. If the divergence is large, it supports treating jurisdictional capture as the dominant rather than incidental function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_security_gap_vs_pretext, empirical, 'Whether the security rationale independently justifies the degree of interpretive transfer, or was pretextual cover for it.').

omega_variable(
    reversibility_of_interpretive_precedent,
    'Can accumulated NPCSC interpretive precedent be narrowed or reversed without formal Basic Law amendment, or does each interpretation function as an irreversible ratchet?',
    'Observe whether any future NPCSC interpretation narrows rather than extends mainland interpretive reach into HK matters, or whether HK courts develop doctrinal techniques (as some common law systems have with supranational law) to cabin the domestic effect of externally-sourced interpretations.',
    'If reversible, fixing_cost may be closer to expensive-but-not-prohibitive; if genuinely ratcheting, the prohibitive classification and the tangled_rope-trending-toward-snare trajectory in the measurements are both reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reversibility_of_interpretive_precedent, empirical, 'Whether the interpretive capture mechanism is structurally reversible or a one-way ratchet.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__jurisdictional_capture_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t0, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(nsl__tr_t12, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement(nsl__tr_t24, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 24, 0.31).
narrative_ontology:measurement(nsl__tr_t36, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 36, 0.36).
narrative_ontology:measurement(nsl__tr_t48, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 48, 0.4).
narrative_ontology:measurement(nsl__tr_t60, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(nsl__be_t0, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(nsl__be_t12, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(nsl__be_t24, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 24, 0.57).
narrative_ontology:measurement(nsl__be_t36, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 36, 0.62).
narrative_ontology:measurement(nsl__be_t48, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 48, 0.66).
narrative_ontology:measurement(nsl__be_t60, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 60, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t0, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(nsl__su_t12, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 12, 0.61).
narrative_ontology:measurement(nsl__su_t24, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(nsl__su_t36, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 36, 0.68).
narrative_ontology:measurement(nsl__su_t48, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 48, 0.7).
narrative_ontology:measurement(nsl__su_t60, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 60, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__jurisdictional_capture_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text__sovereignty_restoration_reading).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text__democratic_enclosure_reading).

% DUAL FORMULATION NOTE:
% Three constraints share the nsl_legal_text kernel and the same statutory text but differ in which harm/benefit structure is treated as primary. jurisdictional_capture_reading (this story) isolates institutional-architecture erosion — common law interpretive autonomy captured by mainland bodies — with HK judiciary/legal profession as victims and mainland security/legal apparatus as beneficiaries; authored as tangled_rope (moderate-high extractiveness, genuine coordination function still present). sovereignty_restoration_reading treats the same text as a legitimate security instrument restoring order after 2019, with correspondingly lower authored extractiveness. democratic_enclosure_reading treats the same text as a permanent dissent-criminalization mechanism, with civil society and political actors as the primary victim set and correspondingly higher authored extractiveness/suppression. Per the ε-invariance principle each reading is a separate ε and a separate file; this story does not average or hedge across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
