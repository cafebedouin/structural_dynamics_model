% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__democratic_enclosure_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nsl_legal_text__democratic_enclosure_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: nsl_legal_text__democratic_enclosure_reading
 *   human_readable: NSL as Democratic Enclosure and Dissent Criminalization (Democratic Reading)
 *   domain: constitutional/political
 *
 * SUMMARY:
 *   The National Security Law (NSL) imposed on Hong Kong in June 2020
 *   establishes a legal framework claimed by Beijing and Hong Kong
 *   authorities as a territorial security instrument restoring constitutional
 *   order. This reading instantiates the NSL as a mechanism for permanent
 *   enclosure of democratic space and criminalization of dissent. The
 *   constraint enters the victim set: civil society, press, opposition,
 *   academics, pro-democracy advocates. The extractiveness is exceptionally
 *   high (0.88): the NSL does not coordinate security alongside preserved
 *   civil liberties — it transfers political authority wholesale to mainland
 *   security doctrine and creates a continuous legal jeopardy for any
 *   organized opposition or independent institutional voice. The suppression
 *   requirement is near-maximal (0.91): the constraint's persistence depends
 *   on an active enforcement apparatus (national security agencies, compliant
 *   courts, informant networks, asset freezes) because the legal threat alone
 *   would generate massive escape attempts without enforcement. The theater
 *   ratio (0.62) reflects that genuine security concerns exist (foreign
 *   interference, separatist agitation) but the constraint's application far
 *   exceeds any proportional response: prosecution of 2019 protest activity,
 *   deregistration of mainstream opposition parties, removal of dissenting
 *   judges — functions that have nothing to do with external security but
 *   everything to do with political control.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__democratic_enclosure_reading, 0.88).
domain_priors:suppression_score(nsl_legal_text__democratic_enclosure_reading, 0.91).
domain_priors:theater_ratio(nsl_legal_text__democratic_enclosure_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 0.91).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, accessibility_collapse, 0.93).
narrative_ontology:constraint_metric(nsl_legal_text__democratic_enclosure_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__democratic_enclosure_reading, snare).
narrative_ontology:human_readable(nsl_legal_text__democratic_enclosure_reading, "NSL as Democratic Enclosure and Dissent Criminalization (Democratic Reading)").
narrative_ontology:topic_domain(nsl_legal_text__democratic_enclosure_reading, "constitutional/political").

domain_priors:requires_active_enforcement(nsl_legal_text__democratic_enclosure_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__democratic_enclosure_reading, '979c0e8b-6170-4401-ae02-04d64fecaba9').
narrative_ontology:cs_kernel_codification('979c0e8b-6170-4401-ae02-04d64fecaba9', formalized).
narrative_ontology:cs_authority_grounding('979c0e8b-6170-4401-ae02-04d64fecaba9', extraction).
narrative_ontology:cs_interpretation_layer_present('979c0e8b-6170-4401-ae02-04d64fecaba9').
narrative_ontology:cs_reading_relation('979c0e8b-6170-4401-ae02-04d64fecaba9', nsl_legal_text__sovereignty_restoration_reading, coexists_with).
narrative_ontology:cs_reading_relation('979c0e8b-6170-4401-ae02-04d64fecaba9', nsl_legal_text__jurisdictional_capture_reading, influences).
narrative_ontology:cs_axiom('979c0e8b-6170-4401-ae02-04d64fecaba9', foundational, democratic_legitimacy_requires_contestation_space).
narrative_ontology:cs_axiom_status(democratic_legitimacy_requires_contestation_space, holdable).
narrative_ontology:cs_axiom_grounding('979c0e8b-6170-4401-ae02-04d64fecaba9', democratic_legitimacy_requires_contestation_space, deontological).
narrative_ontology:cs_axiom('979c0e8b-6170-4401-ae02-04d64fecaba9', foundational, security_law_narrowly_tailored_to_threat).
narrative_ontology:cs_axiom_status(security_law_narrowly_tailored_to_threat, overridden).
narrative_ontology:cs_axiom_grounding('979c0e8b-6170-4401-ae02-04d64fecaba9', security_law_narrowly_tailored_to_threat, empirically_contingent).
narrative_ontology:cs_reference_frame('979c0e8b-6170-4401-ae02-04d64fecaba9', democratic_accountability_framework).
narrative_ontology:cs_drift_state('979c0e8b-6170-4401-ae02-04d64fecaba9', post_nsl_enforcement_stabilization, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('979c0e8b-6170-4401-ae02-04d64fecaba9', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, beijing_central_authority).
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, hong_kong_establishment).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, civil_society_organizations).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, independent_press).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, opposition_political_parties).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, academic_researchers).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, pro_democracy_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nsl_legal_text__democratic_enclosure_reading, ordinary_hong_kong_residents).
narrative_ontology:constraint_victim(nsl_legal_text__democratic_enclosure_reading, ordinary_hong_kong_residents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafts and imposes the NSL, retains interpretive authority over its scope, directs the enforcement apparatus (national security agencies, PCCW liaison offices). Defines what constitutes 'secession,' 'subversion,' 'collusion,' and 'terrorism' according to mainland security doctrine. Faces no domestic legal constraint on the definitions or enforcement.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, beijing_central_authority, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Adopts and administers the NSL through local prosecutorial and judicial machinery. Gains authority to silence opposition, sideline pro-democracy legislators, remove dissenting judges, and consolidate executive power without electoral constraint. Builds a compliant bureaucracy and framed judiciary that defer to mainland security interpretations.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, hong_kong_establishment, beneficiary,
    institutional, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__democratic_enclosure_reading, hong_kong_establishment, agenda_setter).

% Face dissolution, asset freezes, and criminal prosecution for activities that were legal before the NSL: advocacy coalitions, labor unions, religious organizations with political ties, environmental groups, human rights NGOs. Exit via relocation abandons organizational infrastructure and constituent networks; remaining means accepting legal jeopardy for continued work.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, civil_society_organizations, payer,
    organized, generational, identity_locked, regional).

% Editorial teams face charges for reporting on pro-democracy movements, publishing leaked documents, or investigating government misconduct. Newsroom leadership is subject to personal liability; publications can be banned. Closure threats force editorial capitulation (self-censorship) or relocation. The constraint collapses the operational difference between 'reporting the news' and 'committing a state crime.'
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, independent_press, payer,
    moderate, biographical, constrained, regional).

% Are subject to deregistration, member prosecutions, and asset seizures for policy positions (e.g., advocating democratic reforms, universal suffrage, foreign policy independence) that were lawful platforms before the NSL. Cannot exit the political system without abandoning their constituency; continuing to operate means accepting criminal jeopardy for their members.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, opposition_political_parties, payer,
    moderate, biographical, trapped, regional).

% Face prosecution for research into Hong Kong independence movements, China-Hong Kong relations, or the NSL itself. University autonomy shrinks as institutions install mainland-compliant leadership and vet curricula. Researchers publishing critical work face self-exile or employment termination; those remaining enter a surveillance frame where their scholarship is subject to post-hoc criminal reinterpretation.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, academic_researchers, payer,
    moderate, biographical, identity_locked, regional).

% Face the heaviest criminal exposure: protest organizers, strike leaders, and democratic activists are prosecuted under the broadest NSL articles ('subversion,' 'collusion') with sentences reaching life imprisonment. Exit means abandoning their cause and community or physically leaving Hong Kong and losing legal status. Remaining means accepting near-certain prosecution.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, pro_democracy_advocates, payer,
    powerless, biographical, trapped, regional).

% UN rapporteurs, foreign legislatures, international media, and human rights organizations document the NSL's application as de facto criminalization of dissent. They produce evidence of the constraint's structural function (permanent enclosure) but have no enforcement lever over Beijing or Hong Kong authorities.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, international_observers, observer,
    institutional, generational, analytical, global).

% Gain security benefits if they accept the constraint: crime deterrence, order, stability claims. They also live under a pervasive surveillance frame (informant networks, financial monitoring, online speech filtering) and face chilling effects on ordinary discourse. The dual role: passive beneficiary of claimed order, diffuse payer of the surveillance and self-censorship infrastructure.
narrative_ontology:constraint_stakeholder(nsl_legal_text__democratic_enclosure_reading, ordinary_hong_kong_residents, beneficiary,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__democratic_enclosure_reading, ordinary_hong_kong_residents, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nsl_legal_text__democratic_enclosure_reading, beijing_central_authority).
narrative_ontology:fixing_cost_class(nsl_legal_text__democratic_enclosure_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The NSL's stated coordination function: unified security framework defending Hong Kong's territorial integrity and preventing foreign intervention after 2019 civil unrest. The mechanism is to raise the legal and enforcement threshold for political organization and public dissent above the point at which coordinated opposition can operate.
% TRANSFER_FUNCTION: Transfers political authority from electoral/deliberative institutions (legislature, civil service, courts) to the mainland security apparatus and locally compliant executives. Moves civil society organizational capacity (resources, membership, legitimacy) into a criminalized space. Redirects enforcement capacity away from conventional crime and toward political opposition management.
% ABSENT_VOICES: Detained pro-democracy legislators, exiled activists, foreign governments, and international legal bodies that argue the NSL violates the Basic Law and international human rights covenants are structurally excluded from the legislative and enforcement processes that apply it. They would testify that the constraint's function is political enclosure, not security; their absence from the adjudicatory process is itself a structural feature of the constraint.
% DISAPPEARANCE_RATIONALE: If the NSL and its enforcement machinery vanished overnight, civil society organizations would resurface, opposition parties would reconstitute, the independent press would resume critical reporting, and democratic contestation would resume. The constraint is not a background condition of Hong Kong's political economy; it is the active mechanism preventing such reorganization. Its disappearance would reorganize the political order fundamentally.
% FOUNDING_PROBLEM: Hong Kong 2019 civil unrest: anti-extradition bill protests evolved into sustained pro-democracy mobilization challenging the One Country Two Systems settlement. Beijing characterized the movement as secessionist and foreign-instigated; the stated security problem was the threat to territorial integrity and constitutional order.
% FOUNDING_PROBLEM_CORROBORATION: Beijing and the Hong Kong establishment attest the founding problem (unrest, secessionism threat) remains live. Pro-democracy parties and international observers attest that the 2019 unrest was a legitimate response to eroding autonomy and governance legitimacy, not secessionism; they further attest that the NSL's application has criminalized peaceful dissent far beyond any plausible security response to the 2019 events. Independent analysts document the constraint applied retrospectively (prosecuting 2019 protest activity under NSL definitions that did not exist then) and prospectively (chilling lawful speech). The founding-problem reading is corroborated only by the benefiting parties; outside testimony denies that the stated problem matches the NSL's actual enforcement scope.
narrative_ontology:disappearance_verdict(nsl_legal_text__democratic_enclosure_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__democratic_enclosure_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__democratic_enclosure_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(nsl_legal_text__democratic_enclosure_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nsl_legal_text__democratic_enclosure_reading, 0.88, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsl_legal_text__democratic_enclosure_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(nsl_legal_text__democratic_enclosure_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nsl_legal_text__democratic_enclosure_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.88) because the NSL does not solve a genuine coordination problem that all parties benefit from — it unilaterally transfers political control from deliberative/electoral institutions to security apparatus. The constraint extracts political authority, civil society capacity, and opposition voice. Suppression requirement is near-maximal (0.91) because the legal text alone would not sustain the arrangement: without continuous prosecution, informant networks, organizational asset freezes, and institutional compliance, opposition would reorganize rapidly. The theater ratio (0.62) reflects that the NSL maintains the facade of a legitimate security law while its application systematically targets political opposition: the security language (subversion, collusion, terrorism) is real but deployed with far greater intensity against lawful political activity than against any external threat. The measurement series track extraction accumulation and suppression hardening over the interval — extractiveness rises steeply through years 1–3 as the enforcement apparatus operationalizes and stabilizes at 0.88 by year 4, suppression follows a similar trajectory (from 0.78 to 0.91), and theater ratio settles at 0.62 once the constraint reaches steady state. This pattern is consistent with a snare: initial rapid rise as the trap is sprung, then stabilization once the target population learns the new legal boundaries and escape attempts exhaust.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (Beijing/Hong Kong establishment) experiences the NSL as legitimate territorial defense and institutional ordering — they perceive the constraint as restoring constitutional hierarchy after unrest. The payer seats (civil society, opposition, press) experience the same legal text as a mechanism for criminalization and political elimination. The engine computes this divergence from the structural data: Beijing has high-level exit options (arbitrage, can rewrite the law), low directionality (benefits from the arrangement); opposition parties have no exit (trapped, trapped in identity as political organizations), high directionality (extracted from). The divergence is not a disagreement about facts but a structural asymmetry in who can redefine the terms and who lives under the definitions imposed.
 *
 * DIRECTIONALITY LOGIC:
 *   Beijing and the HK establishment are structural beneficiaries: they set the legal text, retain interpretive authority, control the enforcement apparatus, and use the constraint to eliminate political opposition and consolidate control. Their d is near 0.0 (full beneficiary). Civil society, press, opposition, academics, and pro-democracy advocates are structural targets: they bear legal jeopardy, organizational dissolution, asset seizure, and prosecution risk for activities that were lawful before the NSL. Their d approaches 1.0 (full target). Ordinary residents sit near d=0.5 (symmetric): they gain security/order benefits if they accept the constraint, but live under surveillance and chilling effects. The directionality is not derived from beneficiary/victim declarations alone — it follows from the control of definitional authority: whoever can rewrite the legal boundaries, retain interpretive power, and deploy the enforcement apparatus without constraint is the beneficiary; whoever must live within legal boundaries they did not set and cannot challenge is the target.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding-problem analysis resolves the mandatrophy question directly. The NSL was mandated to address the 2019 unrest and prevent secessionism. By this reading, the founding problem's status is 'dead': 2019 unrest has dispersed, protest activity is no longer organized at scale, and the secessionist movement has been criminalized into non-existence. Yet the constraint persists and intensifies — extraction rises to 0.88, suppression remains at 0.91 even though the threat it purportedly defends against has been eliminated. This is the mandatrophy signature: the constraint's justifying problem is gone, the constraint remains, enforcement machinery stays mobilized. The establishment reading (sovereignty_restoration) would deny that the founding problem is dead, asserting that secessionism and foreign interference remain latent threats; the democratic-enclosure reading asserts they are gone and the constraint persists for political control purposes, not security. The mandatrophy_resolved flag should be true for this reading: the founding problem status is authoritatively 'dead' (the opposition organizations have been prosecuted, deregistered, exiled; the unrest has been suppressed), the constraint persists and intensifies, and this divergence is precisely what mandatrophy detection exists to flag.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    security_versus_political_control,
    'Is the NSL''s application pattern consistent with a genuine external security threat (secessionism, foreign interference), or does it reveal a primary function of eliminating domestic political opposition and consolidating control?',
    'Forensic analysis of prosecution patterns: compare NSL charges against external-threat agents (foreign interference, espionage) versus domestic-opposition agents (pro-democracy politicians, civil society organizers). If prosecution volume is disproportionately weighted toward political opposition, the security framing is a cover story.',
    'If the security framing is confirmed (external threats dominate prosecution), the constraint reclassifies toward rope/scaffold (security coordination with asymmetric enforcement). If political opposition dominates, the constraint is confirmed as snare (pure extraction using security language).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(security_versus_political_control, empirical, 'Whether NSL enforcement targets genuine security threats or primarily political opposition.').

omega_variable(
    exit_mechanism_for_constrained_victims,
    'For agents marked identity_locked (civil society organizers, academics, opposition politicians), is the lock functional (they genuinely cannot exit without abandoning their cause/profession) or is it a choice disguised as a structural bind?',
    'Post-exit trajectory analysis: track individuals and organizations that exit Hong Kong (emigrate, disband, go dormant). Do they reconstitute elsewhere and continue their activity, or does the exit make their prior activity impossible? If they reconstitute, the lock was functional (they were unable to exit while maintaining identity in-place). If they dissolve, the exit was available but the cost was unacceptable.',
    'If lock is functional, the suppression mechanism is entirely structural (economic dependency, territorial confinement) and the constraint is pure snare. If lock is chosen (exit available but unacceptable cost), the suppression is partially internalized (the victim accepts legal jeopardy to remain in identity), and the constraint operates as internalized coercion — the extractive force persists even if exit becomes possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_mechanism_for_constrained_victims, empirical, 'Whether identity-lock exits are structurally impossible or choice-costliness that persists even when exit becomes possible.').

omega_variable(
    founding_problem_lifecycle,
    'Did the 2019 unrest and secessionist sentiment genuinely dissipate by year 3–4 of the interval, or does it persist as a latent threat that the constraint addresses?',
    'Public-opinion tracking, protest activity monitoring, and intelligence assessments from independent observers (international media, academic researchers not subject to NSL). If sentiment surveys show declining secessionist support and protest activity drops to background noise, the founding problem is dead. If intelligence assessments report latent separatism and foreign interference networks, the problem persists.',
    'If founding problem is dead, mandatrophy is confirmed (constraint persists without a live founding problem) and the constraint''s function shifts from security to political control. If founding problem persists, mandatrophy is denied and the constraint''s persistence is justified by its founding rationale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_lifecycle, empirical, 'Whether the NSL''s founding problem (2019 unrest, secessionism) is actually dead or persists as latent threat.').

omega_variable(
    kernel_contest_framing,
    'This constraint is one reading of the NSL kernel. The three readings (democratic_enclosure, jurisdictional_capture, sovereignty_restoration) make contradictory claims about the NSL''s function. What makes this reading (democratic enclosure) structurally true rather than the sovereignty-restoration reading?',
    'Comparative analysis of (1) the NSL''s textual scope (does it address only external security or also domestic political activity?), (2) enforcement pattern (what categories of conduct are actually prosecuted?), (3) institutional impact (which institutions lose authority, and to whom?). The reading whose description best fits the observed enforcement pattern and institutional outcomes is structurally true for the constraint''s actual operation.',
    'If enforcement pattern aligns with democratic-enclosure reading, the sovereignty-restoration reading is empirically falsified as a description of what the constraint does (though it may describe what Beijing intended or claims). If enforcement aligns with sovereignty-restoration, this reading is falsified and the constraint operates as institutional security coordination, not democratic enclosure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_contest_framing, empirical, 'Which reading of the NSL kernel describes its actual structural function and enforcement pattern.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__democratic_enclosure_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t0, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement_basis(nsl__tr_t0, observed).
narrative_ontology:measurement(nsl__tr_t6, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 6, 0.52).
narrative_ontology:measurement_basis(nsl__tr_t6, observed).
narrative_ontology:measurement(nsl__tr_t12, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 12, 0.56).
narrative_ontology:measurement_basis(nsl__tr_t12, observed).
narrative_ontology:measurement(nsl__tr_t18, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 18, 0.59).
narrative_ontology:measurement_basis(nsl__tr_t18, observed).
narrative_ontology:measurement(nsl__tr_t24, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 24, 0.61).
narrative_ontology:measurement_basis(nsl__tr_t24, observed).
narrative_ontology:measurement(nsl__tr_t30, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 30, 0.62).
narrative_ontology:measurement_basis(nsl__tr_t30, observed).
narrative_ontology:measurement(nsl__tr_t36, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 36, 0.62).
narrative_ontology:measurement_basis(nsl__tr_t36, observed).
narrative_ontology:measurement(nsl__tr_t42, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 42, 0.62).
narrative_ontology:measurement_basis(nsl__tr_t42, observed).
narrative_ontology:measurement(nsl__tr_t48, nsl_legal_text__democratic_enclosure_reading, theater_ratio, 48, 0.62).
narrative_ontology:measurement_basis(nsl__tr_t48, observed).

% Extraction over time
narrative_ontology:measurement(nsl__be_t0, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement_basis(nsl__be_t0, observed).
narrative_ontology:measurement(nsl__be_t6, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 6, 0.76).
narrative_ontology:measurement_basis(nsl__be_t6, observed).
narrative_ontology:measurement(nsl__be_t12, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 12, 0.81).
narrative_ontology:measurement_basis(nsl__be_t12, observed).
narrative_ontology:measurement(nsl__be_t18, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 18, 0.84).
narrative_ontology:measurement_basis(nsl__be_t18, observed).
narrative_ontology:measurement(nsl__be_t24, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 24, 0.86).
narrative_ontology:measurement_basis(nsl__be_t24, observed).
narrative_ontology:measurement(nsl__be_t30, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 30, 0.87).
narrative_ontology:measurement_basis(nsl__be_t30, observed).
narrative_ontology:measurement(nsl__be_t36, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 36, 0.88).
narrative_ontology:measurement_basis(nsl__be_t36, observed).
narrative_ontology:measurement(nsl__be_t42, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 42, 0.88).
narrative_ontology:measurement_basis(nsl__be_t42, observed).
narrative_ontology:measurement(nsl__be_t48, nsl_legal_text__democratic_enclosure_reading, base_extractiveness, 48, 0.88).
narrative_ontology:measurement_basis(nsl__be_t48, observed).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t0, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 0, 0.78).
narrative_ontology:measurement_basis(nsl__su_t0, observed).
narrative_ontology:measurement(nsl__su_t6, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 6, 0.82).
narrative_ontology:measurement_basis(nsl__su_t6, observed).
narrative_ontology:measurement(nsl__su_t12, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 12, 0.86).
narrative_ontology:measurement_basis(nsl__su_t12, observed).
narrative_ontology:measurement(nsl__su_t18, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 18, 0.88).
narrative_ontology:measurement_basis(nsl__su_t18, observed).
narrative_ontology:measurement(nsl__su_t24, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 24, 0.9).
narrative_ontology:measurement_basis(nsl__su_t24, observed).
narrative_ontology:measurement(nsl__su_t30, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 30, 0.91).
narrative_ontology:measurement_basis(nsl__su_t30, observed).
narrative_ontology:measurement(nsl__su_t36, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 36, 0.91).
narrative_ontology:measurement_basis(nsl__su_t36, observed).
narrative_ontology:measurement(nsl__su_t42, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 42, 0.91).
narrative_ontology:measurement_basis(nsl__su_t42, observed).
narrative_ontology:measurement(nsl__su_t48, nsl_legal_text__democratic_enclosure_reading, suppression_requirement, 48, 0.91).
narrative_ontology:measurement_basis(nsl__su_t48, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__democratic_enclosure_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nsl_legal_text__democratic_enclosure_reading, 0.12).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text__sovereignty_restoration_reading).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, nsl_legal_text__jurisdictional_capture_reading).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, hong_kong_institutional_autonomy).
narrative_ontology:affects_constraint(nsl_legal_text__democratic_enclosure_reading, one_country_two_systems_settlement).

% DUAL FORMULATION NOTE:
% The NSL kernel (nsl_legal_text) instantiates three distinct constraint stories depending on the reading adopted: democratic_enclosure_reading (this file) treats the NSL as a mechanism for permanent closure of democratic space (extractiveness 0.88, snare); sovereignty_restoration_reading treats it as legitimate constitutional restoration after unrest (lower extractiveness, rope/scaffold); jurisdictional_capture_reading treats it as erosion of common-law autonomy (institutional-authority transfer focus). These are not three aspects of one constraint — they are three different constraints instantiated from the same legal text, each with its own ε, beneficiary/victim structure, and classification. They share the kernel but have distinct structural functions and empirical signatures. This reading differs most sharply from sovereignty_restoration on the axis of democratic legitimacy and outcome: the enclosure reading focuses on permanent suppression of opposition; the sovereignty reading focuses on restoration of constitutional order. The institutional target differs from jurisdictional_capture: this reading emphasizes civil-society criminalization; the jurisdictional reading emphasizes legal-system integration.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nsl_legal_text__democratic_enclosure_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
