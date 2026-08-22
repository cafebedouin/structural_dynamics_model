% ============================================================================
% CONSTRAINT STORY: naskh_principle__classical_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__classical_abrogation, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: naskh_principle__classical_abrogation
 *   human_readable: Classical Naskh Abrogation Doctrine
 *   domain: religious/legal/hermeneutic
 *
 * SUMMARY:
 *   The classical naskh doctrine holds that later-revealed Quranic verses
 *   abrogate (invalidate the legal force of) earlier-revealed verses on the
 *   same topic. This reading emerged in the 2nd-3rd Islamic centuries as the
 *   dominant jurisprudential method for resolving apparent textual
 *   contradictions. The constraint operates as a hierarchical interpretive
 *   authority structure: institutional jurists control the chronological
 *   ordering and maintain lists of abrogated/abrogating verses. The doctrine
 *   benefits legal certainty but costs interpretive flexibility; it
 *   coordinates legal output but extracts from contextual and theological
 *   readings. The constraint is CLAIMED as tangled_rope (coordination +
 *   extraction + active enforcement) based on structural analysis: the
 *   doctrine solves the genuine legal coordination problem of resolving
 *   contradictions, but does so through a method that systematically
 *   privileges institutional jurists' interpretive authority, marginalizes
 *   alternative methods, and forecloses readings that would preserve both
 *   verses' legal force. Payers (contextual interpreters, theologians) bear
 *   the cost of methodological delegitimization.
 *
 * KEY AGENTS:
 *   - classical_jurists: institutional power; identity-locked to the abrogation framework; set and enforce the doctrine
 *   - legal_certainty_framework: abstract beneficiary; non-agent; vindicated by the doctrine's operation
 *   - contextual_interpreters: moderate power; constrained exit; pay the cost of marginalization
 *   - theological_coherence_seekers: moderate power; constrained exit; lose the option to preserve all verses as binding
 *   - quranic_traditionalists: powerful; arbitrage exit; benefit from doctrine validation of their transmission
 *   - islamic_legal_institutions: institutional power; agenda-setters and beneficiaries; enforce the doctrine
 *   - reformation_movements: moderate power; identity-locked; excluded from setting jurisprudential direction
 *   - philosophical_theologians: moderate power; constrained exit; historically excluded from mainstream jurisprudence
 *   - historical_critical_scholars: analytical observers; external analysis
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__classical_abrogation, 0.68).
domain_priors:suppression_score(naskh_principle__classical_abrogation, 0.71).
domain_priors:theater_ratio(naskh_principle__classical_abrogation, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, extractiveness, 0.68).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__classical_abrogation, tangled_rope).
narrative_ontology:human_readable(naskh_principle__classical_abrogation, "Classical Naskh Abrogation Doctrine").
narrative_ontology:topic_domain(naskh_principle__classical_abrogation, "religious/legal/hermeneutic").

domain_priors:requires_active_enforcement(naskh_principle__classical_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__classical_abrogation, '90964551-6c96-48d9-ad0e-377cc475a983').
narrative_ontology:cs_kernel_codification('90964551-6c96-48d9-ad0e-377cc475a983', formalized).
narrative_ontology:cs_authority_grounding('90964551-6c96-48d9-ad0e-377cc475a983', lineage).
narrative_ontology:cs_interpretation_layer_present('90964551-6c96-48d9-ad0e-377cc475a983').
narrative_ontology:cs_reading_relation('90964551-6c96-48d9-ad0e-377cc475a983', naskh_principle__contextual_harmonization, forecloses).
narrative_ontology:cs_reading_relation('90964551-6c96-48d9-ad0e-377cc475a983', naskh_principle__progressive_restriction, coexists_with).
narrative_ontology:cs_axiom('90964551-6c96-48d9-ad0e-377cc475a983', foundational, chronological_revelation_supremacy).
narrative_ontology:cs_axiom_status(chronological_revelation_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('90964551-6c96-48d9-ad0e-377cc475a983', chronological_revelation_supremacy, conventional).
narrative_ontology:cs_axiom('90964551-6c96-48d9-ad0e-377cc475a983', foundational, legal_force_invalidation_by_succession).
narrative_ontology:cs_axiom_status(legal_force_invalidation_by_succession, holdable).
narrative_ontology:cs_axiom_grounding('90964551-6c96-48d9-ad0e-377cc475a983', legal_force_invalidation_by_succession, instrumental).
narrative_ontology:cs_reference_frame('90964551-6c96-48d9-ad0e-377cc475a983', chronological_legal_hierarchy).
narrative_ontology:cs_drift_state('90964551-6c96-48d9-ad0e-377cc475a983', contemporary_interpretive_contestation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('90964551-6c96-48d9-ad0e-377cc475a983', '2026-06-19T14:32:00Z').
narrative_ontology:cs_kernel_id(naskh_principle__classical_abrogation, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, classical_jurists).
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, legal_certainty_framework).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, contextual_interpreters).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, theological_coherence_seekers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, quranic_traditionalists).
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, islamic_legal_institutions).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, reformation_movements).
narrative_ontology:constraint_vindicates(naskh_principle__classical_abrogation, chronological_revelation_order_determinacy).
narrative_ontology:constraint_vindicates(naskh_principle__classical_abrogation, legal_ruling_supersession).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Establish and defend the doctrine that later-revealed verses legally abrogate earlier-revealed verses on identical topics. They formalize the criteria for identifying abrogation, maintain the chronological ordering framework, and train students in the jurisprudential method. Their authority derives from lineage transmission of Quranic scholarship and institutional control over Islamic legal pedagogy. The doctrine provides them a systematic method for resolving apparent textual contradictions and buttresses their interpretive authority.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, classical_jurists, agenda_setter,
    institutional, generational, identity_locked, global).

% The abstract doctrine that law requires determinate, non-contradictory rules. The classical abrogation reading benefits this framework by ensuring fixed legal outputs: one rule per legal issue, no superposition. This doctrine's vindication appears in all jurisprudential schools that adopt naskh methodology.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, legal_certainty_framework, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(naskh_principle__classical_abrogation, legal_certainty_framework).

% Scholars and practitioners who believe apparent contradictions in the Quran can be resolved by attending to the specific historical, social, and revelatory contexts in which each verse was revealed, rather than by chronological supersession. They argue that the abrogation doctrine forecloses legitimate interpretive options, reduces the Quran's living guidance to fixed historical rulings, and loses theological richness by flattening contextual specificity into linear succession. They bear the intellectual cost of having their approach marginalized in institutional Islamic legal education.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, contextual_interpreters, payer,
    moderate, biographical, constrained, regional).

% Theologians and philosophers who seek to preserve the Quran as a unified theological whole, viewing apparent contradictions as invitations to deeper understanding rather than textual invalidation. They argue the abrogation doctrine treats the Quranic text as a historical document whose earlier strata are now dead law, rather than as a unified revelation whose every verse retains spiritual and theological authority. They bear the institutional pressure to conform to the dominant abrogation methodology or accept marginalization.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, theological_coherence_seekers, payer,
    moderate, biographical, constrained, regional).

% Classical hadith scholars and Quranic commentators who transmitted and validated the lists of abrogated/abrogating verses. Their scholarly authority and institutional prestige are tied to the coherence of the abrogation framework. They have the power to certify which verses abrogate which, and their transmission chains carry institutional weight in Sunni jurisprudence. They benefit from the doctrine's adoption because it vindicates their historical judgments.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, quranic_traditionalists, beneficiary,
    powerful, generational, arbitrage, global).

% Madrasas, fatwa councils, appellate courts, and mosque authorities that administer Islamic law. They benefit from a fixed, hierarchical system of legal rules; the abrogation doctrine provides them systematic grounds for ruling disputes without contradiction. They enforce the doctrine's use in legal education and fatwa issuance. Their institutional authority depends on the ability to issue determinate judgments, which naskh enables.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, islamic_legal_institutions, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(naskh_principle__classical_abrogation, islamic_legal_institutions, beneficiary).

% Progressive Islamic reform movements that argue the abrogation doctrine locks Islamic law into medieval contexts and prevents contemporary reinterpretation. They see the doctrine as instrumentally enforced to marginalize readings that would permit women's equality, religious pluralism, or democratic governance. They are identity-locked because exit means abandoning the Islamic interpretive tradition itself. They pay the cost of institutional opposition and marginalization.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, reformation_movements, payer,
    moderate, biographical, identity_locked, regional).

% Mutazila and later philosophical movements that historically questioned whether chronological supersession exhausts the relationship between earlier and later verses, or whether logical/philosophical harmonization could preserve both. They are excluded from mainstream institutional Islamic jurisprudence; their objections to naskh remain marginal in Sunni legal pedagogy, though alive in Shia jurisprudence.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, philosophical_theologians, excluded,
    moderate, biographical, constrained, regional).

% Academic specialists in Quranic studies, Islamic history, and comparative theology who examine the abrogation doctrine from outside the jurisprudential framework. They document the doctrine's historical emergence, trace its transmission through classical jurisprudential schools, and analyze its methodological assumptions. They hold no institutional stake in the doctrine's validity but provide external analysis.
narrative_ontology:constraint_stakeholder(naskh_principle__classical_abrogation, historical_critical_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(naskh_principle__classical_abrogation, classical_jurists).
narrative_ontology:fixing_cost_class(naskh_principle__classical_abrogation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves textual contradiction by establishing a hierarchy (later abrogates earlier) that permits jurists to issue determinate legal rulings without superposition or ambiguity. The coordination problem solved: when two Quranic verses appear to command opposite legal outcomes on the same issue, which rule is binding? The abrogation doctrine answers: the later-revealed verse is binding; the earlier verse's legal force is invalidated.
% TRANSFER_FUNCTION: Moves interpretive authority from contextual reasoning (which remains open-ended and contestable) to chronological reasoning (which claims determinacy). The transfer also moves authority over legitimate Islamic legal interpretation from diverse interpreters toward institutional jurists who control the chronological ordering framework and the lists of abrogated/abrogating verses.
% ABSENT_VOICES: Contextual harmonizers and theological pluralists are structurally excluded from setting the jurisprudential agenda. They can publish, teach, and argue, but within institutional Islamic legal education their readings are framed as minority positions, sectarian exceptions, or theological indulgences rather than legitimate jurisprudential methods. The doctrine's enforcement works partly through institutional control: law schools teach naskh as the standard method; alternative methods are taught as historical curiosities or non-Sunni approaches.
% DISAPPEARANCE_RATIONALE: Classical institutional Islam claims the doctrine is necessary for legal coherence and would collapse without it; contextual interpreters argue that the disappearance of naskh would permit richer, more theologically unified readings without loss of practical legal guidance; reformation movements claim the Quranic legal code would actually become more adaptive if freed from the abrogation straightjacket. The world would rearrange in some institutional forms (jurisprudential schools would need alternative harmonization methods) and remain unchanged in others (the Quranic text itself does not depend on naskh to exist).
% FOUNDING_PROBLEM: Early Islamic jurisprudence faced Quranic verses that commanded apparently opposite legal outcomes — e.g., verses permitting wine before later verses forbidding it; verses on divorce and reconciliation; verses on fighting polytheists versus later verses on tolerance toward People of the Book. A systematic method was needed to determine which ruling applied when verses seemed contradictory. The abrogation doctrine provided that method: identify the chronological order of revelation and apply the later ruling.
% FOUNDING_PROBLEM_CORROBORATION: Classical jurists attest the problem is live and abrogation is necessary for legal coherence. Contextual interpreters attest the problem arises from a false assumption that all verses command uniform legal rules; they argue context-specific interpretation resolves the apparent contradictions without invoking abrogation. Historical-critical scholars document that early Islamic jurisprudence did face genuine decisional pressure to resolve contradictions and that naskh was one method among several; they note that Shia jurisprudence, Zaydi jurisprudence, and early Mutazila schools developed alternative systematic methods (e.g., specification/takhsis, context-binding) without invoking abrogation, and these alternatives produced stable jurisprudential systems. Outside the benefiting parties, there is no scholarly consensus that abrogation is the only or best solution to the founding problem.
narrative_ontology:disappearance_verdict(naskh_principle__classical_abrogation, contested).
narrative_ontology:founding_problem_status(naskh_principle__classical_abrogation, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__classical_abrogation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(naskh_principle__classical_abrogation, 'none', 1).
narrative_ontology:epsilon_provenance(naskh_principle__classical_abrogation, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__classical_abrogation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__classical_abrogation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(naskh_principle__classical_abrogation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68) because the doctrine transfers interpretive authority from open-ended contextual reasoning to closed chronological reasoning controlled by institutional jurists. The transfer is net-extractive because alternative methods are not merely de-prioritized but de-legitimated within institutional Islamic law. Suppression is high (0.71) because the doctrine's persistence depends on institutionally excluding or marginalizing contextual approaches in legal education and fatwa issuance. The constraint's enforcement machinery is not coercive (no police force), but pedagogical and institutional: madrasas teach naskh as standard, law school exams test knowledge of the abrogation framework, fatwas are issued through the abrogation method. Theater is moderate (0.42) because the doctrine genuinely resolves legal contradictions (functional core), but an increasing share of institutional activity over time has been devoted to policing the methodological boundary — defending naskh against contextual and philosophical challenges — rather than solving new legal problems. Measurement series spans 1400 years (interval 0–1400, roughly years 0–1400 AH): early emergence (low extractiveness/suppression) as one method among several; crescendo during 4th–8th centuries (rapid rise in both metrics) as naskh became standardized and alternative methods were marginalized; plateau from 8th century onward (metrics flatten) as the doctrine achieved institutional dominance and boundary-policing became routine. The temporal trajectory shows extraction accumulation characteristic of a tangled rope that has been institutionalized.
 *
 * PERSPECTIVAL GAP:
 *   From the classical jurist seat: the doctrine is pure coordination — a systematic method for resolving textual ambiguity and enabling coherent legal development. From the contextual interpreter seat: the same structure operates as enforced extraction — the jurist monopoly on chronological ordering and list-making forecloses legitimate alternatives and extracts authority from more nuanced reasoning. From the legal-certainty framework: the doctrine is a successful vindication — law requires determinate rules and naskh delivers them. From the theological-coherence seat: it is a failure — the doctrine destroys the theological unity the questioner seeks by treating earlier verses as dead law. The engine should compute divergent types across these seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical jurists and institutional Islamic law are beneficiaries (d near 0.0): they control the interpretive method, set the rules for determining chronology, maintain authority through the doctrine, and bear no extraction cost. Contextual interpreters and theological coherence seekers are targets (d near 1.0): they pay an extraction cost (methodological delegitimization, institutional exclusion) and have constrained exit (leaving the tradition means no longer participating in Islamic jurisprudence). Quranic traditionalists are beneficiaries (d near 0.0): their scholarly authority and transmission chains are validated by the doctrine. Reformation movements are targets (d near 1.0): they are identity-locked (exit means leaving Islam) and constrained (institutional pressure to conform or accept marginalization). Historical-critical scholars are analytical (d=0.5): they observe but do not collect or pay extraction within the institutional Islamic framework. The directionality derivation is straightforward: beneficiary/victim declarations map cleanly to exit options and power. No overrides needed.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint solves a real coordination problem (resolving textual contradiction) using a method that extracts from alternative problem-solvers. Mandatrophy arises only if the founding problem dies while the constraint persists. Status quo: the founding problem is contested — classical jurists claim contradictions require abrogation to resolve; contextual interpreters argue context-specific interpretation resolves them without abrogation; historical scholars document that alternative methods (takhsis/specification, context-binding) produced stable jurisprudential systems in Shia and Zaydi traditions. The founding problem has not died; it has been reframed. The constraint does not exhibit mandatrophy in the strict sense (dead founding problem, living extraction). It does exhibit contested-mandate symptoms: the founding problem's necessity is no longer universally accepted, and the doctrine persists through institutional enforcement rather than participant agreement that it solves the problem. The theater-ratio rise (0.18→0.42) tracks the shift from coordination (early emergence) to boundary-policing (institutional defense against alternatives). This is not mandatrophy but mandate-contestation — the constraint is becoming more theatrical as it shifts from problem-solving to authority-maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_necessity,
    'Is chronological abrogation structurally necessary to resolve apparent Quranic contradictions, or are alternative harmonization methods (contextual specification, logical reconciliation, restriction-without-abrogation) sufficient to produce determinate legal rulings?',
    'Comparative analysis of jurisprudential systems: do Shia, Zaydi, Mutazila, and reformist schools that reject or limit naskh produce less coherent, more contradictory legal systems than Sunni schools using classical abrogation? If alternative methods produce equally stable systems, necessity is not established.',
    'If alternative methods are sufficient, the founding problem is solved, and the constraint''s persistence through institutional enforcement becomes evidence of mandatrophy or deliberate authority consolidation. If abrogation is uniquely efficient, the extraction is justified as the cost of coordination. This determines whether the constraint should eventually be reclassified as a zombie or validated as necessary coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_necessity, empirical, 'Whether chronological abrogation is a unique solution to the founding problem or one among multiple sufficient methods.').

omega_variable(
    institutional_suppression_vs_methodological_preference,
    'To what extent does the measured suppression (0.71) reflect active institutional enforcement of the abrogation method versus passive scholarly preference for its clarity and systematic character?',
    'Historical-institutional analysis: does institutional opposition to contextual methods decline when formal legal authority is decentralized? Do regions with less centralized jurisprudential authority show higher rates of contextual interpretation adoption? Post-colonial analysis: when Islamic law operates outside state institutional control, do contextual methods resurge?',
    'High institutional suppression → the constraint operates as a snare (extraction through exclusion). Low institutional suppression → the constraint is a rope (coordination method preferred for its clarity). This determines effective suppression and affects seat-divergent type computation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_suppression_vs_methodological_preference, empirical, 'Whether suppression arises from institutional coercion or scholarly preference for methodological clarity.').

omega_variable(
    reading_boundary_determination,
    'Is the chronological order of revelation (t0 to tn for each verse) an objective historical fact, a textual datum derived from Quranic markers and hadith, or a constructed interpretive framework whose boundaries can be contested?',
    'Quranic scholarship on chronology: Islamic and non-Islamic scholars disagree on the chronological order of 10–15% of verses. Does the doctrine of naskh apply cleanly when chronology is uncertain? When scholars disagree on whether verse A precedes verse B, can the abrogation relationship be determined?',
    'If chronology is objective, abrogation is determinate. If chronology is contested, abrogation becomes contestable, and the constraint''s authority-determining power weakens. This affects accessibility_collapse (how completely alternatives collapse) and the constraint''s effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_determination, conceptual, 'Whether Quranic chronological order is an objective fact or a constructed framework.').

omega_variable(
    preservation_vs_abrogation_theological_semantics,
    'When classical scholarship states that an abrogated verse retains its spiritual/historical value but loses legal force, is this a genuine preservation of theological significance or a rhetorical softening of textual invalidation?',
    'Theological praxis analysis: in institutional Islamic theology education, are abrogated verses taught as rich spiritual/historical resources, or are they treated as superseded and peripheral? Do devotional practices (recitation, memorization, commentary) preserve abrogated verses with equal prominence?',
    'If preservation is genuine, the constraint''s cultural impact is lower (both verses remain meaningful in different registers). If preservation is rhetorical, the constraint extracts more comprehensively — earlier verses are culturally marginalized despite formal preservation claims. This affects resistance and accessibility_collapse metrics.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(preservation_vs_abrogation_theological_semantics, conceptual, 'Whether preserved abrogated verses retain meaningful theological authority or are culturally marginalized.').

omega_variable(
    reading_foreclosure_vs_coexistence,
    'In the same jurisprudential seat, can the classical_abrogation reading and the contextual_harmonization reading coexist, or does commitment to one logically foreclose the other?',
    'Jurisprudential analysis: are there historical or contemporary schools that hold both readings simultaneously (using abrogation for some issues, contextual harmonization for others)? Or is each reading adopted as a complete systematic method, incompatible with the other?',
    'If they can coexist, the reading relation should be ''coexists_with'' rather than ''forecloses'' (would require cs_structure revision). If they genuinely foreclose, the doctrine operates as a stronger institutional constraint because accepting it means rejecting alternatives at the foundation level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_vs_coexistence, conceptual, 'Whether classical abrogation and contextual harmonization can coexist in a single jurisprudential framework or foreclose each other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__classical_abrogation, 0, 1400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nask_tr_t0, naskh_principle__classical_abrogation, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(nask_tr_t0, projected).
narrative_ontology:measurement(nask_tr_t200, naskh_principle__classical_abrogation, theater_ratio, 200, 0.22).
narrative_ontology:measurement_basis(nask_tr_t200, observed).
narrative_ontology:measurement(nask_tr_t400, naskh_principle__classical_abrogation, theater_ratio, 400, 0.32).
narrative_ontology:measurement_basis(nask_tr_t400, observed).
narrative_ontology:measurement(nask_tr_t800, naskh_principle__classical_abrogation, theater_ratio, 800, 0.4).
narrative_ontology:measurement_basis(nask_tr_t800, observed).
narrative_ontology:measurement(nask_tr_t1200, naskh_principle__classical_abrogation, theater_ratio, 1200, 0.41).
narrative_ontology:measurement_basis(nask_tr_t1200, observed).
narrative_ontology:measurement(nask_tr_t1400, naskh_principle__classical_abrogation, theater_ratio, 1400, 0.42).
narrative_ontology:measurement_basis(nask_tr_t1400, observed).

% Extraction over time
narrative_ontology:measurement(nask_be_t0, naskh_principle__classical_abrogation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(nask_be_t0, projected).
narrative_ontology:measurement(nask_be_t200, naskh_principle__classical_abrogation, base_extractiveness, 200, 0.48).
narrative_ontology:measurement_basis(nask_be_t200, observed).
narrative_ontology:measurement(nask_be_t400, naskh_principle__classical_abrogation, base_extractiveness, 400, 0.61).
narrative_ontology:measurement_basis(nask_be_t400, observed).
narrative_ontology:measurement(nask_be_t800, naskh_principle__classical_abrogation, base_extractiveness, 800, 0.66).
narrative_ontology:measurement_basis(nask_be_t800, observed).
narrative_ontology:measurement(nask_be_t1200, naskh_principle__classical_abrogation, base_extractiveness, 1200, 0.67).
narrative_ontology:measurement_basis(nask_be_t1200, observed).
narrative_ontology:measurement(nask_be_t1400, naskh_principle__classical_abrogation, base_extractiveness, 1400, 0.68).
narrative_ontology:measurement_basis(nask_be_t1400, observed).

% Suppression requirement over time
narrative_ontology:measurement(nask_su_t0, naskh_principle__classical_abrogation, suppression_requirement, 0, 0.42).
narrative_ontology:measurement_basis(nask_su_t0, projected).
narrative_ontology:measurement(nask_su_t200, naskh_principle__classical_abrogation, suppression_requirement, 200, 0.51).
narrative_ontology:measurement_basis(nask_su_t200, observed).
narrative_ontology:measurement(nask_su_t400, naskh_principle__classical_abrogation, suppression_requirement, 400, 0.62).
narrative_ontology:measurement_basis(nask_su_t400, observed).
narrative_ontology:measurement(nask_su_t800, naskh_principle__classical_abrogation, suppression_requirement, 800, 0.69).
narrative_ontology:measurement_basis(nask_su_t800, observed).
narrative_ontology:measurement(nask_su_t1200, naskh_principle__classical_abrogation, suppression_requirement, 1200, 0.7).
narrative_ontology:measurement_basis(nask_su_t1200, observed).
narrative_ontology:measurement(nask_su_t1400, naskh_principle__classical_abrogation, suppression_requirement, 1400, 0.71).
narrative_ontology:measurement_basis(nask_su_t1400, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__classical_abrogation, identity_coordination).
narrative_ontology:boltzmann_floor_override(naskh_principle__classical_abrogation, 0.12).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, naskh_principle__contextual_harmonization).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, naskh_principle__progressive_restriction).
narrative_ontology:affects_constraint(naskh_principle__classical_abrogation, islamic_legal_school_boundaries).

% DUAL FORMULATION NOTE:
% The naskh_principle kernel constraint family contains three readings, each with distinct ε and structural implications. Classical_abrogation_reading instantiated here asserts chronological supersession as the legal determinant; contextual_harmonization_reading preserves all verses through context-specification; progressive_restriction_reading reframes abrogation as pedagogical restriction. All three readings operate on the same kernel (Quranic verses and their interrelationship) but produce distinct constraint stories with distinct beneficiary/victim structures and metrics. No single constraint story can adjudicate between readings; the family structure enables corpus-level analysis of how reading choice shapes institutional organization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
