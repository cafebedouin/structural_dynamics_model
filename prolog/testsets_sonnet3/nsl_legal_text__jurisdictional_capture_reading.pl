% ============================================================================
% CONSTRAINT STORY: nsl_legal_text__jurisdictional_capture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: nsl_legal_text__jurisdictional_capture_reading
 *   human_readable: Hong Kong National Security Law as Vehicle for Mainland Legal System Transplantation
 *   domain: constitutional/political/international
 *
 * SUMMARY:
 *   This story reads the 2020 Hong Kong National Security Law as a vehicle
 *   for gradual mainland legal system transplantation — a mechanism that
 *   leaves the common law's institutional shell (courts, barristers, judges)
 *   formally intact while progressively relocating final interpretive
 *   authority, procedural control over an expanding case category, and
 *   normative legal concepts to mainland-controlled bodies. This is one of
 *   three readings of the same kernel text. The
 *   sovereignty_restoration_reading treats the identical text as a legitimate
 *   sovereign security instrument closing a real legislative gap; the
 *   democratic_enclosure_reading treats it as a mechanism for criminalizing
 *   dissent and foreclosing electoral competition. This reading's distinct
 *   empirical claim is neither about legitimacy of the security rationale nor
 *   about democratic space per se, but about jurisdictional architecture:
 *   does adjudicatory authority over an expanding case category structurally
 *   migrate from Hong Kong's common law hierarchy to mainland-controlled
 *   interpretive bodies? The victim set here is specifically institutional —
 *   judiciary, bar, litigants relying on common law predictability — rather
 *   than the political dissidents centered in the enclosure reading, though
 *   the two victim sets overlap substantially in practice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nsl_legal_text__jurisdictional_capture_reading, 0.68).
domain_priors:suppression_score(nsl_legal_text__jurisdictional_capture_reading, 0.71).
domain_priors:theater_ratio(nsl_legal_text__jurisdictional_capture_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(nsl_legal_text__jurisdictional_capture_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nsl_legal_text__jurisdictional_capture_reading, tangled_rope).
narrative_ontology:human_readable(nsl_legal_text__jurisdictional_capture_reading, "Hong Kong National Security Law as Vehicle for Mainland Legal System Transplantation").
narrative_ontology:topic_domain(nsl_legal_text__jurisdictional_capture_reading, "constitutional/political/international").

domain_priors:requires_active_enforcement(nsl_legal_text__jurisdictional_capture_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nsl_legal_text__jurisdictional_capture_reading, '52fd4d9f-0d17-4d77-8b57-8bcafe4d09b8').
narrative_ontology:cs_kernel_codification('52fd4d9f-0d17-4d77-8b57-8bcafe4d09b8', formalized).
narrative_ontology:cs_authority_grounding('52fd4d9f-0d17-4d77-8b57-8bcafe4d09b8', extraction).
narrative_ontology:cs_interpretation_layer_present('52fd4d9f-0d17-4d77-8b57-8bcafe4d09b8').
narrative_ontology:cs_reading_relation('52fd4d9f-0d17-4d77-8b57-8bcafe4d09b8', nsl_legal_text__sovereignty_restoration_reading, coexists_with).
narrative_ontology:cs_reading_relation('52fd4d9f-0d17-4d77-8b57-8bcafe4d09b8', nsl_legal_text__democratic_enclosure_reading, influences).
narrative_ontology:cs_axiom('52fd4d9f-0d17-4d77-8b57-8bcafe4d09b8', foundational, final_interpretive_authority_location_determines_system_membership).
narrative_ontology:cs_axiom_status(final_interpretive_authority_location_determines_system_membership, holdable).
narrative_ontology:cs_axiom_grounding('52fd4d9f-0d17-4d77-8b57-8bcafe4d09b8', final_interpretive_authority_location_determines_system_membership, conventional).
narrative_ontology:cs_axiom('52fd4d9f-0d17-4d77-8b57-8bcafe4d09b8', secondary, jurisdictional_transplantation_can_occur_without_formal_repeal).
narrative_ontology:cs_axiom_status(jurisdictional_transplantation_can_occur_without_formal_repeal, holdable).
narrative_ontology:cs_axiom_grounding('52fd4d9f-0d17-4d77-8b57-8bcafe4d09b8', jurisdictional_transplantation_can_occur_without_formal_repeal, empirically_contingent).
narrative_ontology:cs_reference_frame('52fd4d9f-0d17-4d77-8b57-8bcafe4d09b8', one_country_two_systems_common_law_autonomy).
narrative_ontology:cs_drift_state('52fd4d9f-0d17-4d77-8b57-8bcafe4d09b8', post_2020_nsl_enactment, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('52fd4d9f-0d17-4d77-8b57-8bcafe4d09b8', '').
narrative_ontology:cs_kernel_id(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus).
narrative_ontology:constraint_beneficiary(nsl_legal_text__jurisdictional_capture_reading, office_for_safeguarding_national_security).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_judiciary).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, hong_kong_legal_profession).
narrative_ontology:constraint_victim(nsl_legal_text__jurisdictional_capture_reading, common_law_litigants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A mainland-staffed body operating in Hong Kong with jurisdiction to handle certain national security cases directly under mainland procedure rather than Hong Kong common law procedure, including power to have cases tried on the mainland in specified circumstances. It designates which cases qualify as sufficiently serious or complex to bypass local courts, and its officers are not subject to Hong Kong legal process for actions taken in the exercise of their duties.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, office_for_safeguarding_national_security, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(nsl_legal_text__jurisdictional_capture_reading, office_for_safeguarding_national_security, beneficiary).

% Gains a durable institutional foothold and a legal template inside a jurisdiction that had operated under an entirely separate common law system since 1997. Each case handled under mainland procedure, each certificate issued by the Chief Executive binding courts on questions of fact concerning national security, and each precedent normalizing mainland legal concepts (state secrets, subversion defined broadly) inside Hong Kong's case law extends the reach of mainland legal norms without requiring formal abolition of the common law system.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus, beneficiary,
    institutional, civilizational, analytical, national).

% Common law judges, trained in adversarial procedure, precedent, and judicial independence, now operate under a law that permits the Chief Executive to certify facts as binding, permits handpicked designated judges to hear NSL cases, and permits jury trials to be dispensed with. Judges who wish to remain on the bench must operate within a framework whose interpretive authority sits ultimately with the NPC Standing Committee, not with Hong Kong's own appellate courts. Exit means resignation from the bench or professional marginalization; remaining means adjudicating within a system whose foundational assumptions increasingly diverge from the common law training the judiciary was built on.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_judiciary, payer,
    institutional, generational, constrained, national).

% Barristers and solicitors who built careers on common law advocacy — cross-examination rights, disclosure rules, precedent-based argument — now practice in a system where national security cases follow procedures unfamiliar to common law training, where representing certain defendants invites professional and personal risk, and where the coherence of the profession's own training is undermined case by case. Emigration is the most visible exit and has been substantial, but it means abandoning an entire professional identity and client base built over a career.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, hong_kong_legal_profession, payer,
    organized, biographical, constrained, regional).

% Ordinary parties to legal disputes — commercial and personal — who relied on Hong Kong's common law system for predictable, precedent-bound adjudication as the jurisdiction's core value proposition (to residents, to international business, to the rule-of-law-dependent economy). As NSL-adjacent procedure normalizes exceptions to standard common law guarantees (bail presumptions reversed, jury trials dispensable, certified facts binding), the predictability that made the system valuable erodes for everyone operating within it, not only NSL defendants.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, common_law_litigants, payer,
    powerless, biographical, trapped, local).

% The NPC Standing Committee retains final interpretive authority over the NSL, meaning any judicial reading Hong Kong courts arrive at can be superseded by a mainland legislative body outside the common law's own hierarchy of precedent. This is the structural lever that makes transplantation possible without formal system replacement — the common law forms persist while final authority sits elsewhere.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, beijing_central_authorities, agenda_setter,
    institutional, civilizational, analytical, national).

% Multinational firms and investors who chose Hong Kong specifically for its common law predictability and judicial independence from mainland political direction. They have no voice in NSL's design or interpretation but are directly affected by the erosion of the guarantee they relied on; their practical response is relocation of regional headquarters and legal work to Singapore or elsewhere rather than engagement with the process.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, international_business_community, excluded,
    powerful, biographical, mobile, global).

% Academics tracking the specific mechanisms by which mainland legal concepts and procedures enter Hong Kong case law and administrative practice — documenting citation patterns, procedural departures, and the accretion of NSL-specific exceptions as a case study in legal system transplantation without formal repeal.
narrative_ontology:constraint_stakeholder(nsl_legal_text__jurisdictional_capture_reading, comparative_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(nsl_legal_text__jurisdictional_capture_reading, mainland_security_apparatus).
narrative_ontology:fixing_cost_class(nsl_legal_text__jurisdictional_capture_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides Beijing a mechanism to address what it identifies as a national security gap in a jurisdiction it cannot legislate for directly through its own normal legislative process, without abolishing the common law system by decree — coordination among mainland and Hong Kong security and judicial functions on a defined category of cases.
% TRANSFER_FUNCTION: Moves interpretive and procedural authority over an expanding category of legal questions from Hong Kong's common law judiciary and bar to a mainland-controlled interpretive apparatus (NPC Standing Committee, Office for Safeguarding National Security), while leaving the common law's outward institutional shell — courts, judges, barristers — nominally in place.
% ABSENT_VOICES: Hong Kong's legislature (already reconstituted under a patriots-only electoral framework) has no independent voice in NSL's scope; the pre-2020 pro-democracy legal and political actors most affected by the jurisdictional shift are excluded by definition — disbarred, prosecuted, or exiled — and cannot testify to the transplantation's effect on the system from inside it.
% DISAPPEARANCE_RATIONALE: If the NSL and its jurisdictional carve-outs disappeared overnight, Hong Kong's courts would revert to exclusively common law procedure, the Office for Safeguarding National Security would lose its basis for operating in the territory, mainland legal concepts currently entering Hong Kong case law would halt their accretion, and the profession's emigration pressure tied specifically to NSL practice risk would ease — a substantial rearrangement of who adjudicates what and by what rules.
% FOUNDING_PROBLEM: Framed by its drafters as closing a national security legislative gap Hong Kong's Basic Law Article 23 obligation had left unfulfilled for 23 years, particularly after the 2019 protests were characterized as revealing the territory's vulnerability to secession, subversion, terrorism, and foreign collusion.
% FOUNDING_PROBLEM_CORROBORATION: Beijing and Hong Kong government officials attest the problem (security gap, external interference) remains live and cite ongoing prosecutions as evidence of necessity. Independent bodies outside the benefiting apparatus — the Hong Kong Bar Association's own historical statements prior to restructuring, UN human rights treaty bodies reviewing Hong Kong's compliance reports, and comparative law scholars tracking procedural divergence — corroborate that whatever security gap existed, the law's operation now extends well beyond it into jurisdictional and procedural transplantation with no sunset or narrowing mechanism.
narrative_ontology:disappearance_verdict(nsl_legal_text__jurisdictional_capture_reading, world_rearranges).
narrative_ontology:founding_problem_status(nsl_legal_text__jurisdictional_capture_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nsl_legal_text__jurisdictional_capture_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored moderate-high (0.68 at interval end) reflecting the specific claim that final interpretive authority (NPC Standing Committee override), procedural exceptions (certified facts, dispensable juries, reversed bail presumptions), and an operationally separate security apparatus (Office for Safeguarding National Security) constitute a genuine transfer of jurisdictional control away from Hong Kong's common law hierarchy — not merely enhanced security enforcement within it. Suppression (0.71) is high because the mechanism depends on active enforcement: designated-judge lists, NPC Standing Committee interpretive override, and professional consequences for common law practitioners who resist the shift. Theater ratio (0.4) reflects that courtroom forms — wigs, precedent citation, adversarial procedure — persist substantially unchanged in ordinary cases, which is precisely what makes transplantation-by-accretion rather than abolition-by-decree the mechanism this reading identifies.
 *
 * PERSPECTIVAL GAP:
 *   From the Office for Safeguarding National Security's seat, this is functioning coordination — closing a jurisdictional gap the common law system could not close on its own. From the Hong Kong judiciary's seat, the same structural fact (final interpretive authority sitting outside the common law hierarchy) computes as an ongoing erosion of the professional and institutional foundation their careers were built on. The engine should compute these as structurally different experiences of the identical legal text — that divergence is exactly what this reading isolates for measurement, distinct from the sovereignty or democratic-space readings of the same kernel.
 *
 * DIRECTIONALITY LOGIC:
 *   Mainland security apparatus and the Office for Safeguarding National Security sit at the beneficiary end: they gain jurisdictional reach and a durable institutional foothold without bearing any of the professional or systemic costs. Hong Kong's judiciary, bar, and common law litigants sit at the target end: they experience the same nominal legal system operating under progressively different final-authority rules, with exit meaning career abandonment or jurisdictional flight, not mere inconvenience. Beijing's central authorities are declared agenda_setter with analytical exit because their relationship to the constraint is design and control, not participation subject to its costs or benefits — an institutional actor authoring the rules is not usefully scored on the beneficiary/victim spectrum in the same way as an agent living inside the system.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (a security legislative gap under Basic Law Article 23) may have been genuinely live in 2020; this reading's claim is that the mechanism chosen to close it — jurisdictional transplantation with no sunset, no narrowing clause, and expanding case-category reach — has outlived any plausible scope of the original gap and now operates as a standing capture of adjudicatory authority. founding_problem_status is marked contested rather than dead because the mainland and Hong Kong governments maintain the security rationale remains live; the corroboration and disappearance analysis note that whatever the gap's true extent, the mechanism's operation has moved well past addressing it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transplantation_vs_enforcement_boundary,
    'Is the NSL''s mainland-procedure carve-out best modeled as a bounded, exceptional jurisdiction (limited to a narrow category of the most serious cases) or as an expanding jurisdictional beachhead that will progressively enlarge its case-category reach over time?',
    'Longitudinal tracking of the proportion and character of cases handled under Office for Safeguarding National Security direct jurisdiction versus ordinary Hong Kong prosecution, and whether the boundary criteria (complexity, foreign involvement) are applied narrowly or expansively over successive years.',
    'If bounded and stable, this reading''s extractiveness claim weakens toward the sovereignty_restoration_reading''s framing (a limited security carve-out); if expanding, this reading''s claim strengthens and would support reclassification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transplantation_vs_enforcement_boundary, empirical, 'Whether mainland jurisdictional reach under NSL is bounded or expansionary over time.').

omega_variable(
    common_law_shell_vs_substance,
    'Does the persistence of common law forms (precedent citation, adversarial procedure, wigged barristers) in the large majority of non-NSL Hong Kong litigation mean the common law system substantively survives, or is the NSL carve-out itself sufficient to constitute systemic capture regardless of its case-volume share?',
    'Comparative analysis of jurisdictions with limited national-security carve-outs inside otherwise intact common law systems (e.g., post-9/11 US, UK) against Hong Kong''s specific structure of final interpretive authority sitting with a foreign legislative body.',
    'If a small carve-out with foreign final authority is sufficient to constitute capture regardless of volume, extractiveness and claimed_type toward tangled_rope/snare are well supported; if volume matters more than final-authority location, this reading may overstate present extraction relative to its likely future trajectory.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(common_law_shell_vs_substance, conceptual, 'Whether jurisdictional capture is a function of final-authority location or of case-volume share.').

omega_variable(
    kernel_reading_disaggregation,
    'This story is one reading of the nsl_legal_text kernel (jurisdictional_capture_reading), alongside sovereignty_restoration_reading and democratic_enclosure_reading. Are these genuinely three structurally distinct constraints, or does the jurisdictional-capture claim collapse into a component of the democratic-enclosure claim once political prosecutions and institutional capture are shown to be causally linked?',
    'Track whether NSL cases handled under mainland-influenced procedure are disproportionately political-speech cases (supporting collapse into enclosure reading) or span a genuinely broader category including non-political commercial/security matters (supporting independence of the jurisdictional-capture claim).',
    'If NSL jurisdictional capture is empirically coextensive with political prosecution, the two readings may need to be merged or the jurisdictional-capture reading''s independent ε value reconsidered; if the case category is genuinely broader than political dissent, the readings remain properly distinct per the ε-invariance principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disaggregation, conceptual, 'Whether the jurisdictional-capture and democratic-enclosure readings are empirically separable or coextensive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nsl_legal_text__jurisdictional_capture_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nsl__tr_t0, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(nsl__tr_t12, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement(nsl__tr_t24, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 24, 0.34).
narrative_ontology:measurement(nsl__tr_t36, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 36, 0.37).
narrative_ontology:measurement(nsl__tr_t48, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 48, 0.39).
narrative_ontology:measurement(nsl__tr_t60, nsl_legal_text__jurisdictional_capture_reading, theater_ratio, 60, 0.4).

% Extraction over time
narrative_ontology:measurement(nsl__be_t0, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(nsl__be_t12, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 12, 0.51).
narrative_ontology:measurement(nsl__be_t24, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 24, 0.58).
narrative_ontology:measurement(nsl__be_t36, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 36, 0.62).
narrative_ontology:measurement(nsl__be_t48, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 48, 0.66).
narrative_ontology:measurement(nsl__be_t60, nsl_legal_text__jurisdictional_capture_reading, base_extractiveness, 60, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(nsl__su_t0, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(nsl__su_t12, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 12, 0.62).
narrative_ontology:measurement(nsl__su_t24, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 24, 0.66).
narrative_ontology:measurement(nsl__su_t36, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 36, 0.68).
narrative_ontology:measurement(nsl__su_t48, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 48, 0.7).
narrative_ontology:measurement(nsl__su_t60, nsl_legal_text__jurisdictional_capture_reading, suppression_requirement, 60, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsl_legal_text__jurisdictional_capture_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text__sovereignty_restoration_reading).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, nsl_legal_text__democratic_enclosure_reading).
narrative_ontology:affects_constraint(nsl_legal_text__jurisdictional_capture_reading, hong_kong_judicial_independence_erosion).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings decomposed from the nsl_legal_text kernel per the eps-invariance principle: sovereignty_restoration_reading (Mountain-leaning legitimate-instrument framing, low authored extraction), democratic_enclosure_reading (Snare-leaning political-criminalization framing, high authored extraction targeting dissidents), and jurisdictional_capture_reading (this story — Tangled Rope framing, moderate-high extraction targeting institutional/professional actors). Each carries independent epsilon, beneficiary/victim sets, and claimed_type; they are linked via affects_constraints rather than merged into one observer-relative story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nsl_legal_text__jurisdictional_capture_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
