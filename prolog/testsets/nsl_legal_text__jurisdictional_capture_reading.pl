% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__jurisdictional_capture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nsl_legal_text__jurisdictional_capture_reading
 *   human_readable: NSL as Jurisdictional Capture: Common Law Autonomy Erosion
 *   domain: constitutional/legal/political
 *
 * SUMMARY:
 *   Hong Kong's National Security Law (NSL), enacted in 2020, is read in this
 *   constraint story as a vehicle for the mainland legal system to capture
 *   Hong Kong's autonomous common law jurisdiction. Rather than a security
 *   measure (sovereignty_restoration_reading) or a mechanism for permanent
 *   democratic closure (democratic_enclosure_reading), this reading
 *   emphasizes how NSL transfers judicial authority from Hong Kong courts to
 *   mainland security apparatus, erodes the institutional independence common
 *   law requires, and replaces adversarial common law procedure with mainland
 *   inquisitorial and political-security-driven reasoning. The constraint's
 *   structure is tangled_rope: it coordinates Beijing-Hong Kong security
 *   governance (genuine coordination function) while simultaneously
 *   extracting institutional autonomy from Hong Kong's legal system and legal
 *   profession (asymmetric extraction, victims = judiciary and legal
 *   profession). Active enforcement is required because courts must be
 *   prevented from applying common law standards to NSL cases; without
 *   enforcement, judges would reassert autonomy.
 *
 * KEY AGENTS:
 *   - mainland_security_apparatus: Beijing's security and legal authorities; sets NSL interpretation and enforcement; benefits directly from elimination of judicial uncertainty
 *   - hong_kong_judiciary: retains formal independence but loses authority over NSL cases; faces career jeopardy for unfavorable rulings; theater increases as courts appear to function but do not decide high-stakes cases
 *   - hong_kong_legal_profession: constrained exit; professional traditions erode as practitioners leave or specialize away; common law epistemology displaced by mainland inquisitorial logic
 *   - independent_common_law_tradition: non-agent entity; the institutional practice of common law adjudication, precedent, and adversarial process; scope and authority narrow under NSL
 *   - hong_kong_civil_society: trapped powerless actors; criminalization of dissent and sedition creates chilling effect; many organizations disband or emigrate; structurally excluded from law-making
 *   - international_common_law_practitioners: analytical observers; document the shift and theorize jurisdictional capture; influence external perception but do not participate in the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__jurisdictional_capture_reading, 0.68).
domain_priors:suppression_score(nsl_legal_text__jurisdictional_capture_reading, 0.72).
domain_priors:theater_ratio(nsl_legal_text__jurisdictional_capture_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__jurisdictional_capture_reading, tangled_rope).
narrative_ontology:human_readable(nsl_legal_text__jurisdictional_capture_reading, "NSL as Jurisdictional Capture: Common Law Autonomy Erosion").
narrative_ontology:topic_domain(nsl_legal_text__jurisdictional_capture_reading, "constitutional/legal/political").

domain_priors:requires_active_enforcement(nsl_legal_text__jurisdictional_capture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__jurisdictional_capture_reading, 'b7d62425-d740-4f80-94f5-9ba78a21f7ec').
narrative_ontology:cs_kernel_codification('b7d62425-d740-4f80-94f5-9ba78a21f7ec', fixed_text).
narrative_ontology:cs_authority_grounding('b7d62425-d740-4f80-94f5-9ba78a21f7ec', extraction).
narrative_ontology:cs_interpretation_layer_present('b7d62425-d740-4f80-94f5-9ba78a21f7ec').
narrative_ontology:cs_reading_relation('b7d62425-d740-4f80-94f5-9ba78a21f7ec', nsl_legal_text__sovereignty_restoration_reading, coexists_with).
narrative_ontology:cs_reading_relation('b7d62425-d740-4f80-94f5-9ba78a21f7ec', nsl_legal_text__democracy_enclosure_reading, coexists_with).
narrative_ontology:cs_axiom('b7d62425-d740-4f80-94f5-9ba78a21f7ec', foundational, autonomous_common_law_autonomy_necessary_for_institutional_legitimacy).
narrative_ontology:cs_axiom_status(autonomous_common_law_autonomy_necessary_for_institutional_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('b7d62425-d740-4f80-94f5-9ba78a21f7ec', autonomous_common_law_autonomy_necessary_for_institutional_legitimacy, deontological).
narrative_ontology:cs_axiom('b7d62425-d740-4f80-94f5-9ba78a21f7ec', foundational, legal_system_transplantation_unacceptable_without_consent).
narrative_ontology:cs_axiom_status(legal_system_transplantation_unacceptable_without_consent, holdable).
narrative_ontology:cs_axiom_grounding('b7d62425-d740-4f80-94f5-9ba78a21f7ec', legal_system_transplantation_unacceptable_without_consent, deontological).
narrative_ontology:cs_reference_frame('b7d62425-d740-4f80-94f5-9ba78a21f7ec', common_law_judicial_autonomy).
narrative_ontology:cs_drift_state('b7d62425-d740-4f80-94f5-9ba78a21f7ec', post_nsl_enforcement, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('b7d62425-d740-4f80-94f5-9ba78a21f7ec', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_judiciary).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_legal_profession).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, independent_common_law_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, hong_kong_judiciary).
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, hong_kong_business_sector).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_civil_society).
narrative_ontology:constraint_vindicates(nsl_legal_text__jurisdictional_capture_reading, centralized_legal_authority_doctrine).
narrative_ontology:constraint_vindicates(nsl_legal_text__jurisdictional_capture_reading, national_security_as_sovereign_prerogative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Beijing's security and legal authorities (National Security Commission for Hong Kong and Macau, state security organs, Ministry of Justice) design NSL provisions, approve interpretations, and retain review authority over Hong Kong's NSL prosecutions. They train Hong Kong enforcement actors (police, prosecutors) in mainland security-first reasoning. The apparatus benefits from direct governance power over a previously autonomous jurisdiction and eliminates the institutional risk that Hong Kong courts would dismiss security cases on procedural grounds or civil libertarian reasoning.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus, agenda_setter,
    institutional, generational, arbitrage, global).

% Hong Kong judges retain formal authority to hear and decide cases, including NSL cases, and retain prestige and institutional status. But NSL cases are subject to mainland interpretation guidance transmitted through law ministry channels, internal security vetting of judges assigned to NSL panels, and credible threat of removal from sensitive cases if judges rule in ways Beijing deems unfavorable. Judges face personal and professional jeopardy if they apply common law evidentiary standards or adversarial reasoning to NSL prosecutions. Exit is identity-locked: judicial identity is constitutive of professional self-concept and local social position; emigration or career change is experienced as identity death, not relocation.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_judiciary, payer,
    institutional, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__jurisdictional_capture_reading, hong_kong_judiciary, beneficiary).

% Barristers and solicitors must navigate unpredictable NSL enforcement where clients charged under NSL face barriers to legal representation (intimidation of counsel, reduced access to disclosure, threat of charges against lawyers themselves), potential disbarment for what Beijing deems 'improper defense' of NSL cases, and professional isolation from the common law traditions they trained in. Exit is constrained but not identity-locked: emigration is possible (to Singapore, London, Toronto) and some practitioners choose it, but it means abandoning local practice, client relationships, and market position accumulated over decades. Those who remain face pressure to specialize away from NSL-adjacent work (human rights, dissent defense) or to reposition themselves as intermediaries between Hong Kong and mainland legal cultures.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_legal_profession, payer,
    organized, biographical, constrained, local).

% The institutional practice of common law adjudication — precedent, open court, adversarial process, judicial reasoning as binding constraint on executive power, burden of proof, right to cross-examination — faces structural displacement. NSL cases operate on a different epistemology: security-based reasoning, limited disclosure to defense counsel, political rather than legal grounds for decision (e.g., charges of 'undermining national security' without specification of how the alleged conduct threatens security). The tradition does not cease but its authority and scope narrow; new generations of Hong Kong lawyers inherit a hybrid system where common law tools do not apply to the cases with the highest existential stakes.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, independent_common_law_tradition, payer,
    analytical, civilizational, analytical, local).
narrative_ontology:stakeholder_non_agent(nsl_legal_text__jurisdictional_capture_reading, independent_common_law_tradition).

% NSL articles on sedition, subversion, and foreign collusion criminalize political speech, advocacy, and cross-border organizing that pre-NSL Hong Kong common law permitted. Civil society organizations face existential legal risk; many dissolve, disband, or migrate their operations rather than navigate the prosecutorial uncertainty. The chilling effect operates without trials: the visible prosecutions of protest figures (Joshua Wong, Agnes Chow, and others) and the legal ambiguity of NSL articles (what counts as 'undermining national security'?) create self-censorship among those who remain. This population is excluded from the decision-making that created NSL; their objections (that it erodes common law freedom of speech and assembly) are criminalized rather than heard.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_civil_society, payer,
    powerless, biographical, trapped, local).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__jurisdictional_capture_reading, hong_kong_civil_society, excluded).

% Large corporations, financiers, and real-estate interests benefit from restored political stability (no more massive protests disrupting commerce), simplified regulatory harmonization with mainland China (businesses can plan investment on longer horizons), and preferential treatment in government contracts and licensing for firms with ties to Beijing. Those with mainland connections and willingness to support security enforcement gain competitive advantage. Those without such ties are not persecuted but face subtle discrimination; they retain the option to relocate operations to Singapore or London without legal penalty (mobile exit_options).
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_business_sector, beneficiary,
    powerful, biographical, mobile, global).

% Bar associations in Commonwealth countries, law schools, and comparative legal scholars in the US, UK, Australia, Canada watch Hong Kong as a case study in the displacement of autonomous common law by state-directed legal centralization. They publish analysis, write amicus briefs in cases involving extradition or legal professional liability, and theorize the mechanisms of jurisdictional capture. They are analytical seats: they do not participate in the constraint but measure and interpret it. Their institutional positions as observers give them credibility to challenge Beijing's sovereignty_restoration_reading.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, international_common_law_practitioners, observer,
    institutional, generational, analytical, global).

% Civil liberties advocates and human rights organizations (including lawyers, journalists, academics who prioritize democratic principles) would argue that NSL represents the capture of Hong Kong's legal system by an external security apparatus, eroding the common law autonomy that human dignity and constrained government require. They are structurally excluded from the beneficiary coalition (they do not benefit from Beijing control) and too weak to veto the law; many emigrate rather than practice advocacy under NSL constraints. Their absence from decision-making is structural — the enforcement machinery exists partly to prevent their voices from reaching judges and policymakers.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_civil_libertarians, excluded,
    moderate, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus).
narrative_ontology:fixing_cost_class(nsl_legal_text__jurisdictional_capture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: NSL coordinates security governance and legal enforcement between Beijing and Hong Kong: it aligns prosecutorial discretion so both systems pursue the same targets (dissent, foreign contact, challenges to mainland authority), eliminates the transaction cost that an autonomous Hong Kong judiciary represented (courts could reject mainland definitions of 'national security'), and standardizes what counts as sedition or subversion across the territory. From the mainland security apparatus's structural position, NSL solves a genuine coordination problem: how to govern a jurisdiction with its own courts when those courts might obstruct security operations.
% TRANSFER_FUNCTION: Transfers institutional autonomy and interpretive authority from Hong Kong's common law judiciary and legal profession to mainland security apparatus: Hong Kong judges lose the authority to rule on NSL cases according to common law standards of evidence, procedure, and burden of proof; legal doctrine shifts from adversarial common law epistemology (innocence presumed, prosecution must prove guilt beyond reasonable doubt, cross-examination as truth-testing mechanism) to mainland inquisitorial and political-security-driven logic (security judgment predominates, investigation-based evidence suffices, political questions override legal procedure); legal career paths narrow as practitioners emigrate or specialize away from dissent-adjacent work; transmission of common law reasoning traditions attenuates as the cases with the highest stakes no longer turn on common law tools.
% ABSENT_VOICES: Democratic civil society, human rights advocates, and the Hong Kong legal profession itself would contest NSL as jurisdictional capture rather than security coordination. They would argue that NSL transfers institutional autonomy without consent, that the founding problem (judicial obstruction) was temporary and did not justify permanent legal system transplantation, and that common law autonomy is necessary for human dignity and constrained government. These voices are structurally excluded from the beneficiary coalition and face NSL penalties (prosecution, disbarment, emigration pressure) for public dissent; their exclusion is what the constraint's enforcement machinery exists to enforce.
% DISAPPEARANCE_RATIONALE: If NSL disappeared, Hong Kong's courts would reassert common law standards for criminal procedure, evidence, and judicial reasoning; prosecutions deemed politically motivated would face higher evidentiary bars and would be dismissed if evidence falls below common law standards; the legal profession would reunify around common law practice norms and would mentor new lawyers in adversarial reasoning rather than security-driven compliance; civil society would mobilize again knowing that dissent would not be prosecuted under vague NSL articles; and the pressure toward mainland legal harmonization would lose its statutory anchor. Hong Kong would reorganize around legal autonomy and common law traditions rather than security centralization and mainland epistemology.
% FOUNDING_PROBLEM: The 2019 Hong Kong protests created mainland anxiety that Hong Kong's autonomous judiciary would dismiss security cases on procedural grounds (requiring proper evidence, allowing cross-examination of security witnesses, suppressing coerced confessions) or on civil libertarian reasoning (interpreting 'sedition' and 'subversion' narrowly, protecting political speech and protest as common law permits). Beijing feared that judges trained in common law would see NSL-equivalent prosecutions as politically motivated rather than legally justified, and would exclude them or acquit defendants on technical grounds. NSL was enacted to prevent autonomous courts from acting as a check on security enforcement.
% FOUNDING_PROBLEM_CORROBORATION: Beijing security analysts and allied legal scholars in mainland China attest the founding problem is live and ongoing: Hong Kong remains a potential center of dissent (anti-mainland advocacy continues despite prosecutions), courts must be prevented from becoming 'tools of hostile forces,' and NSL enforcement must continue and potentially expand. Hong Kong legal profession representatives and international human rights bodies attest the founding problem was a temporary political crisis (2019 unrest with its peak in 2020) whose legitimate resolution required law enforcement, police reform, and dialogue — not permanent legal system transplantation. Law review articles and legislative testimony from the Hong Kong Bar Association, the Law Society of Hong Kong, and international bar associations document how the stated problem (judicial obstruction of security) was a cover for jurisdictional capture and how NSL's language (sedition, subversion, foreign collusion) far exceeds what is necessary to prosecute actual national security threats. The British Foreign Office and international legal commentators corroborate the capture reading by contrasting NSL with the common law sedition statutes it nominally replaces — NSL's vagueness and the elimination of evidentiary safeguards are distinctive features of mainland law, not common law tradition.
narrative_ontology:disappearance_verdict(nsl_legal_text__jurisdictional_capture_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__jurisdictional_capture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__jurisdictional_capture_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(nsl_legal_text__jurisdictional_capture_reading, 'none', 1).

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
 *   Extractiveness is moderate-high (0.68 at interval end) because NSL captures a core institutional power — the authority to interpret and apply criminal law — and transfers it from Hong Kong to mainland control. This is not merely redistributing resources; it is the displacement of one legal epistemology (common law) by another (mainland inquisitorial/political). Suppression is higher (0.72) because the constraint's persistence depends on actively preventing courts from reasserting autonomy — without enforcement, judges would rule on NSL cases according to common law standards. Theater rises from 0.22 to 0.41 over the interval as Hong Kong courts continue to operate and hear cases, but the appearances mask the loss of decision-making authority on matters that trigger existential stakes (prosecution of dissent, sedition, foreign contact). The measurement series tracks the trajectory of institutional capture: initial extractiveness is lower because the constraint is new and judges still exercise some residual discretion; as enforcement mechanisms mature and judges internalize the risks of unfavorable rulings, extractiveness asymptotes toward the structural maximum. Theater rises because the courts' performative legitimacy becomes more important as their actual authority shrinks — the appearance of independent adjudication must be maintained while actual decisions are predetermined or reviewed by mainland apparatus.
 *
 * PERSPECTIVAL GAP:
 *   The mainland security apparatus and the Hong Kong judiciary compute radically different constraint types from the same legal text. From Beijing's seat, NSL is rope — genuine coordination that solves a real security problem (judicial obstruction) while benefiting all parties through stability and rule clarity. From the judiciary's seat, NSL is tangled_rope transitioning toward snare — the coordination benefit is negligible (they had no role in creating the problem) and the extraction is total (loss of appellate authority). From the legal profession's seat, the constraint is closer to snare — no coordination benefit accrues to lawyers, only the extraction of autonomy and the chilling effect on practice. The engine computes these divergences from the structural data (power, time_horizon, exit_options, beneficiary/victim declarations); the authored claim (tangled_rope) reflects the constraint's objective structure, while the per-seat computations will reveal the asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Mainland security apparatus sits at d near 0.0 (full beneficiary): it collects the institutional authority transfer, faces no exit costs, and retains the power to reshape the constraint. Hong Kong judiciary sits at d near 0.95 (nearly full target): it loses decision-making authority over NSL cases, faces career jeopardy for unfavorable rulings, and cannot exit without abandoning professional identity — hence identity_locked exit_options. Legal profession sits at d near 0.85: similar extraction, somewhat more mobile (can emigrate) but constrained by professional identity and local ties. Civil society sits at d = 1.0 (complete target): criminalized, trapped, no exit short of complete removal from Hong Kong. Overrides should not be necessary because the structural derivation from beneficiary/victim + exit captures the true directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was Beijing's anxiety that an autonomous Hong Kong judiciary would dismiss security cases on procedural grounds — a real institutional conflict, not a fabricated mandate. But the founding problem status is contested: Hong Kong legal analysts and international observers argue the 2019 unrest was a temporary political crisis whose legitimate resolution did not require permanent legal system transplantation. From mainland analysts' perspective, the problem is live and ongoing (dissent has not ceased; courts remain a potential obstacle). The constraint persists because it benefits the beneficiary set (Beijing security apparatus, partially Hong Kong business) and because the suppression mechanisms are strong enough to prevent the victim set from reforming it. If the founding problem genuinely resolved (dissent ended, courts fully compliant), the theater_ratio would be expected to rise sharply while extractiveness remained flat — the courts would continue to operate but only as performance, not as sites of actual decision-making. The current trajectory shows both theater and extractiveness rising, which suggests the founding problem is being redefined: no longer 'judicial obstruction of security cases' but 'maintenance of ideological conformity' — a mandate creep that indicates mandatrophy is in progress.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_epistemological_capture,
    'Does NSL capture Hong Kong''s legal system by transplanting mainland inquisitorial epistemology, or does the capture operate purely through institutional control (mainland appointments to adjudicate NSL cases) while Hong Kong courts retain common law reasoning authority?',
    'Close reading of NSL-case judicial reasoning and comparison of decisions to pre-NSL Hong Kong common law standards; ethnographic study of judicial training and mentorship networks post-NSL; interviews with judges about reasoning constraints.',
    'If epistemological capture is complete, extractiveness should be re-measured higher — the loss is not just institutional authority but the cognitive frameworks through which law is interpreted. If institutional capture is separable from epistemological capture, extractiveness might be lower and theater_ratio significantly higher (courts retain reasoning but not decision authority).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_epistemological_capture, empirical, 'Degree to which NSL captures epistemology (how law is reasoned) vs. structure (who decides).').

omega_variable(
    judicial_complicity_vs_coercion,
    'To what extent do Hong Kong judges internalize mainland security reasoning and voluntarily apply it, vs. comply under external coercion and personal risk?',
    'Post-constraint-exit interviews with emigrated judges; analysis of written judicial opinions for signs of internalized vs. forced reasoning; legal psychology and organizational behavior evidence about norm internalization under pressure.',
    'If internalization is substantial, the constraint has achieved suppression through identity-fusion rather than raw coercion — judges come to see NSL as legitimate, reducing the need for enforcement and moving the constraint toward piton-like persistence (theater dominates). If coercion dominates, suppression_requirement remains high and the constraint is more fragile (depends on continued enforcement machinery).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_complicity_vs_coercion, empirical, 'Degree to which judicial compliance is internalized vs. externally coerced.').

omega_variable(
    common_law_tradition_transmission,
    'Can common law traditions be transmitted and maintained in Hong Kong under NSL constraints, or does the constraint systematically dissolve the transmission mechanisms (mentorship, appellate reasoning, professional networks)?',
    'Generational study of law school curricula and student outcomes; tracking of barristers and solicitors entering NSL-sensitive practice areas; analysis of case law to measure continuity of precedent reasoning vs. doctrinal rupture.',
    'If transmission is systematically blocked, the constraint is not merely extractive in one generation but genocidal (in the institutional sense) to the common law tradition — future Hong Kong law will inherit a hybrid system where common law tools are atrophied. This would suggest the constraint''s type may be under-measured in extractiveness (the loss extends to future generations, not just current institutional autonomy).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(common_law_tradition_transmission, empirical, 'Whether NSL preserves or destroys the mechanisms that transmit common law traditions.').

omega_variable(
    mainland_legal_system_convergence,
    'Is NSL a one-way transplantation of mainland law into Hong Kong, or does Hong Kong law gradually influence mainland legal practice, creating hybrid or convergent legal systems?',
    'Historical analysis of post-constraint legal case outcomes; interviewing legal professionals in both systems about borrowing and influence; analysis of legal doctrine publications in both jurisdictions.',
    'One-way transplantation supports the snare reading and indicates high extraction with no reciprocal benefit to Hong Kong legal autonomy. Mutual influence would suggest a more genuinely hybrid rope — the common law tradition is not destroyed but transformed through contact. But mutual influence would require Hong Kong to retain enough institutional authority to influence mainland practice, which NSL''s structure seems designed to prevent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mainland_legal_system_convergence, empirical, 'Whether legal system transplantation is unidirectional or mutualistic.').

omega_variable(
    kernel_reading_contestation,
    'NSL text is interpreted differently by Beijing (sovereignty_restoration), Hong Kong civil society (democratic_enclosure), and international legal scholars (jurisdictional_capture). Do these readings represent different legitimate interpretations of an ambiguous text, or does the text clearly privilege one reading and others are cover stories?',
    'Linguistic analysis of NSL statutory language and drafting history; comparison of how NSL has been applied in practice vs. how each reading would predict it would be applied; judicial opinions that explicitly address the readings.',
    'If the text is genuinely ambiguous, the readings coexist as live interpretive options, and which one prevails is decided by power and enforcement. If the text clearly encodes the jurisdictional_capture_reading but Beijing presents the sovereignty_restoration reading, that is a falsification indicator and suggests the constraint persists partly through cover narratives. Either way, the presence of competing readings is itself a sign the kernel is contested and subject to normative revision.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Whether NSL''s statutory meaning is ambiguous across readings or encodes one reading and obscures others.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__jurisdictional_capture_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t0, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(nsl__tr_t0, observed).
narrative_ontology:measurement(nsl__tr_t3, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 3, 0.28).
narrative_ontology:measurement_basis(nsl__tr_t3, observed).
narrative_ontology:measurement(nsl__tr_t6, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 6, 0.33).
narrative_ontology:measurement_basis(nsl__tr_t6, observed).
narrative_ontology:measurement(nsl__tr_t10, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 10, 0.38).
narrative_ontology:measurement_basis(nsl__tr_t10, observed).
narrative_ontology:measurement(nsl__tr_t15, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 15, 0.4).
narrative_ontology:measurement_basis(nsl__tr_t15, observed).
narrative_ontology:measurement(nsl__tr_t20, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 20, 0.41).
narrative_ontology:measurement_basis(nsl__tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(nsl__be_t0, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(nsl__be_t0, observed).
narrative_ontology:measurement(nsl__be_t3, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 3, 0.58).
narrative_ontology:measurement_basis(nsl__be_t3, observed).
narrative_ontology:measurement(nsl__be_t6, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 6, 0.62).
narrative_ontology:measurement_basis(nsl__be_t6, observed).
narrative_ontology:measurement(nsl__be_t10, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement_basis(nsl__be_t10, observed).
narrative_ontology:measurement(nsl__be_t15, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement_basis(nsl__be_t15, observed).
narrative_ontology:measurement(nsl__be_t20, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(nsl__be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t0, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(nsl__su_t0, observed).
narrative_ontology:measurement(nsl__su_t3, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 3, 0.63).
narrative_ontology:measurement_basis(nsl__su_t3, observed).
narrative_ontology:measurement(nsl__su_t6, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 6, 0.67).
narrative_ontology:measurement_basis(nsl__su_t6, observed).
narrative_ontology:measurement(nsl__su_t10, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement_basis(nsl__su_t10, observed).
narrative_ontology:measurement(nsl__su_t15, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 15, 0.71).
narrative_ontology:measurement_basis(nsl__su_t15, observed).
narrative_ontology:measurement(nsl__su_t20, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement_basis(nsl__su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__jurisdictional_capture_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(nsl_legal_text__jurisdictional_capture_reading, 0.12).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text__democracy_enclosure_reading).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text__sovereignty_restoration_reading).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, hong_kong_rule_of_law_institutional_independence).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, hong_kong_legal_profession_exit_dynamics).

% DUAL FORMULATION NOTE:
% The NSL kernel has three distinct constraint readings: (1) sovereignty_restoration_reading (NSL as legitimate security instrument) — a mountain or rope from Beijing's structural position; (2) democratic_enclosure_reading (NSL as mechanism for permanent democratic closure) — a snare focused on speech criminalization; (3) jurisdictional_capture_reading (this story) — NSL as vehicle for legal system transplantation and institutional capture. The three readings differ in beneficiary set, victim set, and ε-value because they emphasize different aspects of NSL's operation. This story (jurisdictional_capture_reading) extracts from the legal profession and judiciary; democratic_enclosure_reading extracts from civil society and dissent; sovereignty_restoration_reading claims no extraction (pure coordination). Each reading is a separate constraint with its own classification. The three stories form a constraint family linked by network.affects_constraints edges: capture reading affects both democratic enclosure reading (captures the legal mechanisms used for enclosure) and sovereignty restoration reading (provides counterargument to legitimacy claim).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
