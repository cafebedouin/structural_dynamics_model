% ============================================================================
% CONSTRAINT STORY: qualified_immunity_doctrine__constitutional_fidelity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_qualified_immunity_doctrine__constitutional_fidelity_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: qualified_immunity_doctrine__constitutional_fidelity_reading
 *   human_readable: Qualified Immunity Doctrine — Constitutional Fidelity Reading
 *   domain: constitutional law / civil rights / law enforcement policy
 *
 * SUMMARY:
 *   This file instantiates ONE reading — the constitutional_fidelity_reading
 *   — of the qualified_immunity_doctrine kernel, and generates a clean,
 *   epsilon-invariant constraint for that reading alone. The standing
 *   arrangement under contest (the epsilon referent for every reading of this
 *   kernel) is the operative immunity standard in federal civil-rights
 *   litigation: a body of judicial rulings, originating in Pierson v. Ray
 *   (1967) and recast in Harlow v. Fitzgerald (1982), that shields individual
 *   officials from personal damages liability unless the plaintiff can match
 *   their conduct to prior cases deemed 'clearly established,' applied as a
 *   pre-trial filter across the civil-rights caseload. Assessed by this
 *   reading's own lights, the arrangement is a judicial fabrication: its text
 *   appears in no statute and no constitutional clause, Congress enacted the
 *   underlying remedial statute with no immunity provision, and the
 *   doctrine's legitimacy is therefore void regardless of whether its
 *   screening function protects officers well or badly. The reading locates
 *   the primary beneficiary set in the judiciary itself — each refinement of
 *   the standard extends the circumstances under which the court, rather than
 *   enacted law, defines official protections — with officers receiving
 *   incidental shelter under rules no legislature gave them, and plaintiffs
 *   bearing systematic remedy denial. CONSTRAINT-FAMILY NOTE: the kernel
 *   decomposes into three sibling stories sharing this referent with
 *   reading-indexed epsilon — protective_scaffold_reading (epsilon assessed
 *   as necessary protection), accountability_void_reading (epsilon assessed
 *   as impunity machinery), and this file (epsilon assessed against the
 *   enacted-statutory baseline the doctrine displaces). The values differ
 *   because the readings differ, not because the arrangement differs;
 *   cross-file comparison is the diagnostic. CLAIM/METRIC INDEPENDENCE is
 *   preserved: claimed_type tangled_rope states the functional structure this
 *   reading observes (a real screening coordination function grafted to
 *   asymmetric extraction, held by continuous judicial enforcement); the
 *   metrics describe operation; the reading's normative verdict lives in
 *   cs_structure.axioms and the omegas, not in tuned numbers.
 *
 * KEY AGENTS:
 *   - supreme_court: agenda-setter and primary beneficiary (institutional / identity_locked) — authors and maintains the standard; collects doctrinal authority and interpretive discretion
 *   - lower_federal_courts: beneficiary and day-to-day administrator (institutional / constrained) — applies the pre-trial filter, feeds the precedent library the matching test draws on
 *   - constitutional_tort_plaintiffs: primary target (powerless / trapped) — bears remedy denial for claims matched out before trial
 *   - individual_law_enforcement_officers: incidental beneficiary and quasi-target (organized / constrained) — shielded from personal liability, governed by an unpredictable standard no legislature enacted
 *   - congress: excluded authorizer and structural target of the power transfer (institutional / constrained) — holds unused formal power to define or abolish official immunities
 *   - municipal_indemnifiers: secondary beneficiary (institutional / mobile) — payout and defense-spending exposure suppressed by early dismissal
 *   - constitutional_law_scholars: analytical observer (moderate / analytical) — document the provenance record and propose replacements; reach the doctrine only through the courts that choose to read them
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.7).
domain_priors:suppression_score(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.62).
domain_priors:theater_ratio(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(qualified_immunity_doctrine__constitutional_fidelity_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(qualified_immunity_doctrine__constitutional_fidelity_reading, tangled_rope).
narrative_ontology:human_readable(qualified_immunity_doctrine__constitutional_fidelity_reading, "Qualified Immunity Doctrine — Constitutional Fidelity Reading").
narrative_ontology:topic_domain(qualified_immunity_doctrine__constitutional_fidelity_reading, "constitutional law / civil rights / law enforcement policy").

domain_priors:requires_active_enforcement(qualified_immunity_doctrine__constitutional_fidelity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(qualified_immunity_doctrine__constitutional_fidelity_reading, '3509d3d7-213b-46ff-88d8-ab85b7308407').
narrative_ontology:cs_kernel_codification('3509d3d7-213b-46ff-88d8-ab85b7308407', fixed_text).
narrative_ontology:cs_authority_grounding('3509d3d7-213b-46ff-88d8-ab85b7308407', lineage).
narrative_ontology:cs_interpretation_layer_present('3509d3d7-213b-46ff-88d8-ab85b7308407').
narrative_ontology:cs_reading_relation('3509d3d7-213b-46ff-88d8-ab85b7308407', qualified_immunity_doctrine__protective_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('3509d3d7-213b-46ff-88d8-ab85b7308407', qualified_immunity_doctrine__accountability_void_reading, coexists_with).
narrative_ontology:cs_axiom('3509d3d7-213b-46ff-88d8-ab85b7308407', foundational, official_immunity_requires_express_authorization).
narrative_ontology:cs_axiom_status(official_immunity_requires_express_authorization, holdable).
narrative_ontology:cs_axiom_grounding('3509d3d7-213b-46ff-88d8-ab85b7308407', official_immunity_requires_express_authorization, conventional).
narrative_ontology:cs_axiom('3509d3d7-213b-46ff-88d8-ab85b7308407', secondary, policy_outcomes_cannot_launder_unauthorized_power).
narrative_ontology:cs_axiom_status(policy_outcomes_cannot_launder_unauthorized_power, holdable).
narrative_ontology:cs_axiom_grounding('3509d3d7-213b-46ff-88d8-ab85b7308407', policy_outcomes_cannot_launder_unauthorized_power, deontological).
narrative_ontology:cs_reference_frame('3509d3d7-213b-46ff-88d8-ab85b7308407', enacted_statutory_remedial_scheme).
narrative_ontology:cs_drift_state('3509d3d7-213b-46ff-88d8-ab85b7308407', contemporary_post_harlow_era, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('3509d3d7-213b-46ff-88d8-ab85b7308407', '').
narrative_ontology:cs_kernel_id(qualified_immunity_doctrine__constitutional_fidelity_reading, qualified_immunity_doctrine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, supreme_court).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, lower_federal_courts).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, individual_law_enforcement_officers).
narrative_ontology:constraint_beneficiary(qualified_immunity_doctrine__constitutional_fidelity_reading, municipal_indemnifiers).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, constitutional_tort_plaintiffs).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, congress).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(qualified_immunity_doctrine__constitutional_fidelity_reading, individual_law_enforcement_officers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authors and periodically reshapes the immunity standard for officials sued under the civil-rights statute, deciding case-by-case when a plaintiff may recover damages. Each refinement extends the circumstances in which the court, rather than Congress or the constitutional text, defines what protections officials enjoy. The institution cannot readily abandon the standard it authored without conceding that earlier generations of its own rulings lacked foundation; its authority rests on presenting its rulings as principled continuation rather than invention.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, supreme_court, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__constitutional_fidelity_reading, supreme_court, beneficiary).

% Apply the immunity standard daily in civil-rights cases, using it as a pre-trial filter that removes a large share of complaints before full proceedings. They inherit refinements from above and contribute case-specific precedents that feed the prior-case library the matching test draws on. Deviating from the standard invites reversal from above; applying it consumes far less court time than processing claims to completion.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, lower_federal_courts, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__constitutional_fidelity_reading, lower_federal_courts, agenda_setter).

% Sue officials over concrete rights violations — beatings, wrongful shootings, unlawful detentions — seeking compensatory damages. Their claims routinely die before trial when no prior case happens to match their circumstances closely enough to count as clearly established. They cannot take the claim elsewhere: no other forum pays damages for federal constitutional violations by individual officers, and an appeal re-enters the same standard in the same courts.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, constitutional_tort_plaintiffs, payer,
    powerless, biographical, trapped, national).

% Receive personal-liability shielding under the standard and rely on employer indemnification for residual exposure. The same standard governs them through an unpredictable case-matching test they cannot reliably anticipate, and it was never enacted by any legislature whose authority they answer to; the rules shaping their liability shift with each judicial refinement, and their professional associations press for the standard's retention while absorbing its unpredictability.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, individual_law_enforcement_officers, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__constitutional_fidelity_reading, individual_law_enforcement_officers, payer).

% Enacted the civil-rights remedial statute whose text nowhere authorizes an official immunity standard, and retains sole formal power to define or abolish such protections by law. Its repeated reform output — hearings, committee reports, House-passed legislation — does not enter the judicial conversation that maintains the standard; the standard exists in the space its non-response left open, and the definition of official protections has been transferred out of its hands without its consent.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, congress, excluded,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(qualified_immunity_doctrine__constitutional_fidelity_reading, congress, payer).

% City governments and their insurers who pay when an official is found liable. Early dismissal of claims under the standard cuts their payout exposure and litigation defense spending; they support the standard's retention and adjust their risk pricing around its behavior.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, municipal_indemnifiers, beneficiary,
    institutional, generational, mobile, national).

% Publish the critique record: tracing the doctrine's creation to specific opinions, documenting the absence of statutory anchoring, and proposing replacements ranging from statutory codification to outright removal. Their work supplies the evidentiary basis that legislators and dissenting justices cite, but it reaches the doctrine only through the courts that choose to read it.
narrative_ontology:constraint_stakeholder(qualified_immunity_doctrine__constitutional_fidelity_reading, constitutional_law_scholars, observer,
    moderate, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(qualified_immunity_doctrine__constitutional_fidelity_reading, supreme_court).
narrative_ontology:fixing_cost_class(qualified_immunity_doctrine__constitutional_fidelity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Filters civil-rights damage suits against individual officials before full trial proceedings, centralizing in the judiciary the determination of when official conduct warrants personal-liability protection, so that officials acting in legally unsettled situations are not personally ruined by litigation over judgments no legislature had pre-approved.
% TRANSFER_FUNCTION: Moves compensatory-damages value away from plaintiffs whose claims fail the prior-case matching test toward protected officials and their indemnifiers; and moves lawmaking authority over official protections from Congress and the enacted text to the interpreting judiciary.
% ABSENT_VOICES: Plaintiffs whose claims died at the matching test — the arrangement's largest cost-bearing population — generate no seated voice: dismissed complainants rarely publish, sustain appeals, or organize formally. Congress holds formal authorization power but its reform output does not enter the judicial conversation. Both populations would object to the standard's current shape; neither is seated in the process that maintains it.
% DISAPPEARANCE_RATIONALE: Pending civil-rights cases would reorganize overnight around a different liability standard; officer employers would reprice indemnification; courts would lose their highest-volume pre-trial filter and process far more claims to completion; and Congress would immediately regain the immunity-definition question it currently cannot reach. The remedial economy of civil-rights litigation visibly depends on the arrangement.
% FOUNDING_PROBLEM: Officials faced personal ruin from damages suits over good-faith judgments made under legally unsettled conditions, chilling enforcement; the enacted remedial statute's text was silent on immunities, and the judiciary filled the silence judicially beginning in 1967.
% FOUNDING_PROBLEM_CORROBORATION: Officer associations and indemnifying governments attest the litigation-burden problem from inside the beneficiary set. Outside it: cross-ideological legal scholarship corroborates that the underlying problem existed while documenting that the judicial response outran it; published statements from dissenting justices concede the doctrine's lack of statutory foundation; legislative hearing testimony from both parties acknowledges officer litigation burdens. Corroboration that THIS arrangement is the needed solution comes almost exclusively from the benefiting parties — stated plainly, no external source attests the arrangement itself as necessary.
narrative_ontology:disappearance_verdict(qualified_immunity_doctrine__constitutional_fidelity_reading, world_rearranges).
narrative_ontology:founding_problem_status(qualified_immunity_doctrine__constitutional_fidelity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(qualified_immunity_doctrine__constitutional_fidelity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(qualified_immunity_doctrine__constitutional_fidelity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(qualified_immunity_doctrine__constitutional_fidelity_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(qualified_immunity_doctrine__constitutional_fidelity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(qualified_immunity_doctrine__constitutional_fidelity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(qualified_immunity_doctrine__constitutional_fidelity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.70) is measured against the enacted remedial baseline this reading takes as the referent: the statute's core promise was compensatory access for constitutional violations, and the matching test removes a large share of meritorious claims before trial, transferring that remedial value to protected officials and indemnitors while transferring the immunity-definition decision itself from Congress to the bench. Suppression (0.62) is authored as a RAW STRUCTURAL PROPERTY — unscaled by power or scope in the engine's arithmetic; only extractiveness is scaled. Its substance is procedural: sua sponte invocation, reversal-threat discipline on deviating lower courts, and closure of the legislative exit (reform output cannot reach a standard the courts alone maintain). Theater (0.46): the 'clearly established law' inquiry performs jurisprudential rigor while operating as case-by-case similarity grading, and the retained good-faith vocabulary performs a protection rationale that Harlow abandoned when it discarded the subjective test — real screening nonetheless occurs, so theater stays below half. Accessibility collapse (0.50): alternatives persist (injunctive and declaratory routes, municipal-entity liability, state tort claims) but the individual-officer damages route — the statute's central remedial channel — collapses for most plaintiffs. Resistance (0.68): unusually broad for a legal doctrine — reconsideration calls from within the Court itself, House-passed reform legislation, sustained cross-ideological scholarship, and state-level departures; plaintiffs are individually powerless, so this figure substantially reflects coalition capacity (advocacy organizations, bar networks, legislative allies) rather than isolated victim pushback. Trajectory: the series ratchets at 1982 (Harlow's objective test removed the good-faith escape hatch), steps again with the sequencing experiments of 2001 and their 2009 partial retreat, and hardens post-2015 as the matching standard tightened; all three tracked metrics share one eight-point grid so no metric row is sampled against another metric's end-state.
 *
 * PERSPECTIVAL GAP:
 *   Four seats experience one structure. From the supreme_court seat the standard is principled continuity — cautious stewardship of officials caught between enacted text and practical governance; computed from a beneficiary declaration with identity-locked exit, that seat classifies near the coordination pole. From the constitutional_tort_plaintiffs seat the same rulings are remedy confiscation — a matching test no complainant can argue with, applied by the only forum that can pay; trapped exit and powerless power push that seat to the extraction pole. From the officers' seat it is shelter with a price: protection delivered through a standard whose content shifts beneath them and whose authority no legislature supplied. From the congress seat it is expropriation of a formal power the constitutional order reserves to enactment — the strongest institutional actor in the system rendered structurally voiceless on this specific question. The engine computes these divergences from the declared roles, power atoms, and exits; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations drive the derivation. supreme_court and lower_federal_courts carry beneficiary declarations with identity-locked and constrained exits respectively — d sits near the subsidized pole; their collected rent is lawmaking authority and docket relief. municipal_indemnifiers: beneficiary, mobile exit — strongly subsidized. constitutional_tort_plaintiffs: victim declaration, powerless, trapped — d near the full-target pole, with national scope amplifying effective extraction. congress: victim-declared through its payer secondary role despite institutional power — the doctrine extracts precisely the prerogative (immunity definition) that enactment reserves to Congress, so high d coexists with high power; this is the story's cleanest demonstration that directionality tracks structural relationship, not global strength. individual_law_enforcement_officers carry a deliberate override (organized -> 0.32): the bare beneficiary declaration would derive roughly 0.10-0.15, but their situation includes real cost-bearing — an unenacted, case-matched governing standard, career and reputational exposure, zero legitimate-framework footing — placing them materially above pure subsidy though far below the plaintiffs' pole. No override touches the institutional atoms: courts, municipalities, and Congress differentiate cleanly through their role declarations, which is exactly what the derivation chain is for.
 *
 * MANDATROPHY ANALYSIS:
 *   Classification discipline prevents two opposite mislabels. Reading the doctrine as pure extraction would erase the genuine screening function — courts do remove frivolous and duplicative claims — and would misdirect reform toward bare abolition when replacement (statutory codification of a bounded immunity) addresses both halves. Reading it as pure coordination would launder the extraction and, worse for this reading, launder the provenance defect: a coordination arrangement's legitimacy comes from solving a collective problem, and this arrangement's authorization deficit is exactly what a coordination framing conceals. The tangled-rope claim keeps both halves visible and lets the engine price each seat separately. On mandate obsolescence: the declared mandate — spare good-faith officials from ruinous suits over legally unsettled acts — has been overtaken by the doctrine's own operation, which was rebuilt in 1982 to ignore good faith entirely and which now blocks even clearly-established violations pending proof; the mandate outlived the function it named, hence mandatrophy_resolved is declared, while the founding problem itself is contested rather than dead (officer-side attestations keep the underlying litigation-burden concern alive).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This constraint is one reading (constitutional_fidelity) of the qualified_immunity_doctrine kernel; what do the sibling readings'' structural deltas look like over the identical referent, and where exactly does the contest sit?',
    'Compile the three sibling stories sharing this kernel referent and compare computed per-seat classifications; divergence in victim sets, beneficiary sets, and legitimacy verdicts locates the disagreement structurally.',
    'If the protective_scaffold reading computes coordination-dominant and this reading computes extraction-dominant over the same arrangement, the kernel''s classification is reading-indexed rather than intrinsic — cross-reading comparison becomes the primary diagnostic, and no single type verdict attaches to ''qualified immunity'' as a label.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame routing: one kernel, three readings, per-reading constraints with per-reading epsilon over a shared referent.').

omega_variable(
    authorization_dispositiveness,
    'Is absence of constitutional or statutory authorization dispositive of the doctrine''s structural verdict, or can a six-decade judicial practice accrete conventional legitimacy that changes the classification independently of provenance?',
    'Doctrinal-historical analysis separating adoption mechanics (court initiative versus congressional acquiescence) from mere duration; test whether acquiescence-without-authorization is structurally distinguishable from authorization, e.g., by whether Congress retains live formal power to override.',
    'If conventional accretion counts as legitimacy, the doctrine migrates toward constructed-but-functional coordination and this reading''s verdict softens; if not, the arrangement remains usurpation regardless of age, and the extracted good is the enacted remedial scheme itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authorization_dispositiveness, conceptual, 'Whether longevity without authorization converts judicial fabrication into settled convention.').

omega_variable(
    judiciary_benefit_primacy,
    'Is the judiciary''s institutional power expansion the operative benefit sustaining the doctrine, or is it incidental to a genuine officer-protection demand that the judiciary merely supplies?',
    'Counterfactual supply test: if Congress codified a bounded immunity statute tomorrow, does judicial insistence on the judicially-authored version persist? Persistence beyond statutory substitution indicates power-expansion primacy; ready substitution indicates service delivery.',
    'Primacy confirms the judiciary as the gain_flow capturer and elevates effective extraction on the judicial seats; incidence flips the beneficiary structure toward officers and indemnifiers and repositions this reading closer to the accountability_void sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judiciary_benefit_primacy, empirical, 'Whether the doctrine''s persistence serves judicial power or officer protection demand.').

omega_variable(
    reform_absorption_capacity,
    'Will the mounting cross-ideological resistance (House-passed reform legislation, justice-level reconsideration calls, state-level departures) dissolve the doctrine, or be absorbed into interpretive refinement that leaves the substance intact?',
    'Track legislative passage and Supreme Court reconsideration over the coming decade; absorption manifests as refinement-without-substance-change (e.g., discretionary adjustments to the matching standard or the sequencing protocol) while the authorization deficit remains untouched.',
    'Absorption sustains the enforcement-dependent classification and predicts further extraction accumulation; genuine dissolution would force reclassification toward transitional status during whatever statutory replacement follows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_absorption_capacity, empirical, 'Whether the resistance wave reforms, absorbs, or dissolves the doctrine.').

omega_variable(
    epsilon_framework_circularity,
    'Does measuring epsilon require first settling which framework governs (the enacted statutory remedial scheme versus the doctrine as accomplished institutional fact), making epsilon assessment circular within this reading?',
    'Hold the referent fixed at the standing arrangement and vary only the reading, comparing sibling files'' epsilon over the identical referent; if epsilon moves only with the reading, the circularity is real and measurable as reading-indexed variance.',
    'If circularity holds, cross-reading epsilon differences measure the readings rather than the doctrine, and all classification comparisons in this kernel family must be conducted reading-indexed; treating any single file''s verdict as the doctrine''s verdict would be a category error.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epsilon_framework_circularity, conceptual, 'Whether epsilon measurement in this kernel is framework-circular and therefore inherently reading-indexed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(qualified_immunity_doctrine__constitutional_fidelity_reading, 1967, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qi_fidelity_tr_t1967, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 1967, 0.15).
narrative_ontology:measurement(qi_fidelity_tr_t1976, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 1976, 0.18).
narrative_ontology:measurement(qi_fidelity_tr_t1982, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 1982, 0.3).
narrative_ontology:measurement(qi_fidelity_tr_t1991, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 1991, 0.33).
narrative_ontology:measurement(qi_fidelity_tr_t2001, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 2001, 0.38).
narrative_ontology:measurement(qi_fidelity_tr_t2009, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 2009, 0.4).
narrative_ontology:measurement(qi_fidelity_tr_t2018, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 2018, 0.44).
narrative_ontology:measurement(qi_fidelity_tr_t2026, qualified_immunity_doctrine__constitutional_fidelity_reading, theater_ratio, 2026, 0.46).

% Extraction over time
narrative_ontology:measurement(qi_fidelity_be_t1967, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 1967, 0.3).
narrative_ontology:measurement(qi_fidelity_be_t1976, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 1976, 0.34).
narrative_ontology:measurement(qi_fidelity_be_t1982, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 1982, 0.52).
narrative_ontology:measurement(qi_fidelity_be_t1991, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 1991, 0.56).
narrative_ontology:measurement(qi_fidelity_be_t2001, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 2001, 0.6).
narrative_ontology:measurement(qi_fidelity_be_t2009, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 2009, 0.63).
narrative_ontology:measurement(qi_fidelity_be_t2018, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 2018, 0.67).
narrative_ontology:measurement(qi_fidelity_be_t2026, qualified_immunity_doctrine__constitutional_fidelity_reading, base_extractiveness, 2026, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(qi_fidelity_su_t1967, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 1967, 0.25).
narrative_ontology:measurement(qi_fidelity_su_t1976, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 1976, 0.28).
narrative_ontology:measurement(qi_fidelity_su_t1982, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 1982, 0.45).
narrative_ontology:measurement(qi_fidelity_su_t1991, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 1991, 0.48).
narrative_ontology:measurement(qi_fidelity_su_t2001, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 2001, 0.52).
narrative_ontology:measurement(qi_fidelity_su_t2009, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 2009, 0.55).
narrative_ontology:measurement(qi_fidelity_su_t2018, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 2018, 0.6).
narrative_ontology:measurement(qi_fidelity_su_t2026, qualified_immunity_doctrine__constitutional_fidelity_reading, suppression_requirement, 2026, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(qualified_immunity_doctrine__constitutional_fidelity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, protective_scaffold_reading).
narrative_ontology:affects_constraint(qualified_immunity_doctrine__constitutional_fidelity_reading, accountability_void_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the qualified_immunity_doctrine kernel decomposes into three reading-stories sharing one referent (the doctrine as operated) with reading-indexed epsilon. This file (constitutional_fidelity) links both siblings via affects_constraints; the upstream/downstream structure runs through the authorization premise — this reading's provenance critique supplies the legitimacy frame within which the accountability_void reading's consequence critique is mounted, while the protective_scaffold reading occupies the opposing legitimacy frame. Cross-file epsilon comparison over the fixed referent is the family's diagnostic; no single file's verdict is the doctrine's verdict.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(qualified_immunity_doctrine__constitutional_fidelity_reading, organized, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
