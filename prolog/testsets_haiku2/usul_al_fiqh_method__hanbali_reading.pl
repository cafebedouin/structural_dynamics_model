% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_usul_al_fiqh_method__hanbali_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: usul_al_fiqh_method__hanbali_reading
 *   human_readable: Hanbali Methodological Restriction: Textual Supremacy and Innovation Blocking
 *   domain: legal/religious
 *
 * SUMMARY:
 *   The Hanbali methodological reading of usul al-fiqh (the roots/principles
 *   of Islamic jurisprudence) establishes textual sources—the Qur'an and
 *   authenticated hadith—as maximally restrictive and complete; analogical
 *   reasoning (qiyas) is minimized to cases of clear textual silence; weak
 *   hadith is preferred over reasoning by analogy; and innovation (bid'a) is
 *   blocked through sadd al-dhara'i (closing the means to harm). This reading
 *   stands in structural tension with three sibling readings: the Hanafi
 *   reading, which permits expansive qiyas and juristic preference; the
 *   Maliki reading, which grants independent evidentiary weight to Medinan
 *   practice and public interest; and the Shafi'i reading, which systematizes
 *   authentication of hadith as prerequisite. The Hanbali reading is one
 *   instantiation of contested jurisprudential authority in Islamic law. The
 *   constraint is CLAIMED as a tangled rope (genuine coordination function +
 *   enforced asymmetry) while the authored metrics describe substantial
 *   extraction and high suppression—the engine measures that divergence.
 *
 * KEY AGENTS:
 *   - textualist_legal_scholars: institutional beneficiaries whose authority depends on textual completeness
 *   - orthodoxy_maintenance_institutions: agenda-setters enforcing the restriction against bid'a
 *   - rationalist_jurists: powerful but constrained targets who bear the cost of delegitimization
 *   - customary_practice_communities: identity-locked targets whose traditional legal arrangements face doctrinal override
 *   - public_interest_centered_interpreters: moderate-power targets whose juristic tools (maslaha, sadd al-dhara'i as expansion) are subordinated to textual gates
 *   - prophetic_transmission_specialists: organized beneficiaries whose authentication expertise becomes prerequisite
 *   - competing_methodological_schools: excluded powerful actors whose foundational principles are contested
 *   - islamic_legal_theory_academics: observational analysts of the methodological record
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanbali_reading, 0.68).
domain_priors:suppression_score(usul_al_fiqh_method__hanbali_reading, 0.71).
domain_priors:theater_ratio(usul_al_fiqh_method__hanbali_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, resistance, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanbali_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanbali_reading, "Hanbali Methodological Restriction: Textual Supremacy and Innovation Blocking").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanbali_reading, "legal/religious").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanbali_reading, '3a33056e-8cc7-4339-b94e-a20f39f122f9').
narrative_ontology:cs_kernel_codification('3a33056e-8cc7-4339-b94e-a20f39f122f9', fixed_text).
narrative_ontology:cs_authority_grounding('3a33056e-8cc7-4339-b94e-a20f39f122f9', lineage).
narrative_ontology:cs_interpretation_layer_present('3a33056e-8cc7-4339-b94e-a20f39f122f9').
narrative_ontology:cs_reading_relation('3a33056e-8cc7-4339-b94e-a20f39f122f9', usul_al_fiqh_method__hanafi_reading, forecloses).
narrative_ontology:cs_reading_relation('3a33056e-8cc7-4339-b94e-a20f39f122f9', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('3a33056e-8cc7-4339-b94e-a20f39f122f9', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_axiom('3a33056e-8cc7-4339-b94e-a20f39f122f9', foundational, textual_sources_are_complete).
narrative_ontology:cs_axiom_status(textual_sources_are_complete, holdable).
narrative_ontology:cs_axiom_grounding('3a33056e-8cc7-4339-b94e-a20f39f122f9', textual_sources_are_complete, deontological).
narrative_ontology:cs_axiom('3a33056e-8cc7-4339-b94e-a20f39f122f9', foundational, bid_a_is_categorically_impermissible).
narrative_ontology:cs_axiom_status(bid_a_is_categorically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('3a33056e-8cc7-4339-b94e-a20f39f122f9', bid_a_is_categorically_impermissible, theological).
narrative_ontology:cs_axiom('3a33056e-8cc7-4339-b94e-a20f39f122f9', secondary, qiyas_requires_explicit_textual_silence).
narrative_ontology:cs_axiom_status(qiyas_requires_explicit_textual_silence, holdable).
narrative_ontology:cs_axiom_grounding('3a33056e-8cc7-4339-b94e-a20f39f122f9', qiyas_requires_explicit_textual_silence, deontological).
narrative_ontology:cs_reference_frame('3a33056e-8cc7-4339-b94e-a20f39f122f9', textual_completeness_doctrine).
narrative_ontology:cs_drift_state('3a33056e-8cc7-4339-b94e-a20f39f122f9', contemporary_novel_case_pressures, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3a33056e-8cc7-4339-b94e-a20f39f122f9', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, textualist_legal_scholars).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, orthodoxy_maintenance_institutions).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, customary_practice_communities).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, public_interest_centered_interpreters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, public_interest_centered_interpreters).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, prophetic_transmission_specialists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Islamic jurisprudents who hold that textual sources (Quran and authenticated hadith) contain the complete legal and ethical guidance needed. The Hanbali method validates their interpretive authority and the closure of legal derivation against rationalist expansion. Their scholarly legitimacy depends on the assumption that innovation (bid'a) is categorically impermissible and that textual sources admit of no gaps.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, textualist_legal_scholars, beneficiary,
    institutional, generational, identity_locked, continental).

% Institutional bodies (madrasas, fatwa councils, state-backed ulama hierarchies) that enforce doctrinal coherence by adjudicating permissibility. They set the standard for what counts as valid legal derivation and what is condemned as bid'a. The Hanbali method gives them a framework with high legibility and strong gatekeeping: fewer sources to interpret, minimal discretion, strong enforcement against rationalist and customary expansions.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, orthodoxy_maintenance_institutions, agenda_setter,
    institutional, generational, arbitrage, continental).

% Islamic jurisprudents who defend qiyas (analogical reasoning) and ra'y (reasoned opinion) as valid sources when textual silence obtains. They argue that analogical derivation and juristic preference (istihsan) are necessary to extend law to novel cases and address public interest. Under the Hanbali reading, their methods are delegitimized as rationalist over-reach and sources of bid'a; their work faces institutional pressure and is classified as methodologically suspect.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, rationalist_jurists, payer,
    powerful, biographical, constrained, continental).

% Muslim communities whose legal practices are rooted in local custom ('urf), Medinan practice, and established tradition. Under the Hanbali reading, these practices are vulnerable to challenge as inconsistent with textual sources; the method privileges authenticated hadith over weak hadith and custom, making local legal arrangements subject to doctrinal override if they cannot trace directly to strong textual authority.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, customary_practice_communities, payer,
    moderate, generational, identity_locked, regional).

% Jurisprudents and institutional actors who invoke maslaha mursala (unrestricted public interest) and sadd al-dhara'i (blocking of means to harm) as independent sources or as justification for departure from textual strictness. The Hanbali reading treats these as subordinate to textual authority; maslaha must be constrained by what texts permit, and sadd al-dhara'i becomes a mechanism for blocking innovation rather than enabling juristic expansion in the public interest.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, public_interest_centered_interpreters, payer,
    moderate, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanbali_reading, public_interest_centered_interpreters, beneficiary).

% Hadith scholars and muhaddithin whose expertise is authentication of hadith chains and texts. The Hanbali preference for authenticated hadith over qiyas and the openness to weak hadith (when it does not contradict strong hadith) elevates their role as gatekeepers of what counts as valid law-grounding. Their work becomes the prerequisite for all legal derivation, concentrating interpretive authority in their hands.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, prophetic_transmission_specialists, beneficiary,
    organized, generational, mobile, continental).

% Hanafi, Maliki, and Shafi'i schools whose methodologies emphasize greater role for qiyas, customary practice, and public interest reasoning. They are institutionally present but their foundational principles are characterized as methodologically less rigorous or prone to bid'a; their legitimacy as legal methodologies is contested within the framework the Hanbali reading sets.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, competing_methodological_schools, excluded,
    powerful, generational, constrained, continental).

% Comparative legal scholars and usul al-fiqh specialists who analyze the methodological differences across schools. They observe the constraint's operation as part of the historical jurisprudential record and can document how the Hanbali reading has been interpreted, applied, and contested across centuries.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, islamic_legal_theory_academics, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__hanbali_reading, orthodoxy_maintenance_institutions).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__hanbali_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a systematic procedure for deriving Islamic law from sources, preventing unlimited rationalist expansion and maintaining doctrinal coherence by anchoring all derivation to authenticated texts and minimizing discretionary reasoning.
% TRANSFER_FUNCTION: Transfers methodological legitimacy and interpretive authority from rationalist jurists and customary legal practitioners to textualist scholars and hadith specialists; also transfers the power to adjudicate permissibility from localized practice to centralized doctrinal institutions.
% ABSENT_VOICES: Philosophers and scientific rationalists who might argue for integration of empirical reasoning into legal method; lay communities whose customary practices are overridden; Qur'anic interpreters (mufassirun) who read textual meanings expansively; sufis and spiritual practitioners whose juristic innovations are classified as bid'a.
% DISAPPEARANCE_RATIONALE: If the Hanbali methodological restriction disappeared, legal derivation across Islamic jurisprudence would immediately re-open qiyas, maslaha, and customary sources; institutional gatekeeping on bid'a would lose its textualist frame; rationalist jurisprudents would recover authority; local practices would claim independent evidentiary weight rather than facing doctrinal challenge. The landscape of legitimate legal development would shift within a generation.
% FOUNDING_PROBLEM: Early Islamic jurisprudence risked unlimited rationalist expansion and locally idiosyncratic custom undermining coherence of shari'a; protecting textual sources from dilution by opinion and custom preserves the revelation's integrity and prevents each region or jurist from deriving incompatible rules.
% FOUNDING_PROBLEM_CORROBORATION: Textualist scholars attest the founding problem remains live, citing instances of juridical divergence and rationalist overreach in contemporary fatwa. Hanafi and Maliki scholars contest whether the problem is as severe as textualists claim and whether textual sources alone address novel issues in changing times. Comparative legal analysts document instances where Hanbali restriction blocked adaptation to novel cases, supporting the view that the founding problem—rigidity—persists alongside the original founding problem.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanbali_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanbali_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanbali_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(usul_al_fiqh_method__hanbali_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__hanbali_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(usul_al_fiqh_method__hanbali_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(usul_al_fiqh_method__hanbali_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(usul_al_fiqh_method__hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.68 at interval end, rising from 0.48) captures the growing enforcement of the textual-restrictiveness claim as institutional gatekeeping strengthens over the interval. The suppression score (0.71 at end) reflects the sustained pressure required to block rationalist expansion, customary practice, and public-interest reasoning—a constraint that persists by actively excluding alternatives, not by participant preference. The theater ratio (0.42 at end, rising from 0.28) indicates that enforcement activity increasingly focuses on the performative exclusion of competing methodologies rather than on the core coordination function (doctrinal coherence). The accessibility_collapse (0.79) is high because once a scholar accepts that all law is determined by texts, the alternative approaches (qiyas, custom, maslaha) become rationally illegible within that frame. The resistance (0.61) is substantial because rationalist and customary jurists mount continuous counter-arguments and institutional competition across the interval. The time-series measurements show extraction and suppression both rising asymptotically toward 0.68–0.71 by the midpoint and plateauing thereafter—a pattern consistent with institutional enforcement hardening early, then stabilizing once the gatekeeping apparatus is mature and resistance is absorbed into academic controversy rather than institutional challenge. All measurements share the same time grid (0, 8, 16, 24, 32, 40, 50).
 *
 * PERSPECTIVAL GAP:
 *   From the textualist-scholar and institutional-gatekeeper seats, the Hanbali reading is genuine coordination: a principled procedure preventing infinite juristic divergence and protecting revealed sources from dilution. From the rationalist-jurist and customary-community seats, the same structure operates as enforced extraction: legitimate juristic tools are declared methodologically suspect and local practice is overridden by doctrinal fiat. From the public-interest-centered seat, the constraint performs double work—coordinating against chaotic expansion while also subordinating maslaha to textual gates, making it a partial payer and partial beneficiary. The engine computes these divergences per-seat from the structural data; no single authored claim adjudicates across them.
 *
 * DIRECTIONALITY LOGIC:
 *   Textualist scholars and orthodoxy institutions benefit (d near 0.0) by consolidating interpretive authority and closing legal derivation against their competitors. Rationalist jurists and customary communities are targets (d near 1.0) because their methods are delegitimized and their practices face institutional pressure. Prophetic-transmission specialists benefit moderately (d near 0.2) by becoming prerequisite gatekeepers. Public-interest interpreters sit near the symmetric point (d near 0.5): they coordinate on a genuine doctrinal problem (avoiding chaotic expansion) but also lose independent justification for maslaha-driven reasoning. The competing schools are excluded rather than coordinated—their exclusion is the enforcement mechanism itself. The overrides in directionality are minimal here because the structural data (beneficiaries, victims, power atoms) derive cleanly to the observed directionality spread.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (rationalist expansion and custom-driven divergence undermine revealed law's integrity) is declared 'contested'—textualists say it remains live, while Hanafi and Maliki schools argue the problem is overstated and the Hanbali solution creates new rigidity. The disappearance verdict is 'world_rearranges'—the legal landscape would shift toward reopened qiyas and customary authority. The mismatch (founding_problem_status=contested + disappearance_verdict=world_rearranges) signals that the constraint's mandate is contested, not obsolete: if the founding problem were clearly dead, world_rearranges would be overdetermined; if it were clearly live and severe, the other schools would not contest its characterization. The rising theater_ratio (0.28→0.42 over the interval) combined with plateauing suppression (stabilizing at 0.71) suggests the constraint has moved from active enforcement against live rationalist challenge toward maintenance-theater: the gatekeeping institutions continue performance of doctrinal purity, but the rationalist jurists have become an academic opposition rather than an institutional threat. This pattern is consistent with a constraint whose original function (blocking bid'a-driven legal chaos) has succeeded in establishing institutional orthodoxy, but whose persistence beyond that success requires increasingly performative reinforcement. No mandatrophy resolution is claimed; the constraint remains active but the contested founding-problem status and rising theater signal an asymptote of diminishing returns on enforcement intensity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_completeness_vs_novel_cases,
    'Are the Qur''an and authenticated hadith genuinely complete as sources of legal guidance for all novel cases, or does the closure of qiyas and maslaha create gaps where communities must either invoke weak hadith or override local practice?',
    'Historical documentation of cases where Hanbali jurisprudents resorted to weak hadith or implicit maslaha despite the methodological prohibition; contemporary instances where Hanbali communities adopt heterodox solutions to novel problems; comparative legal analysis of what happens to novel cases under each methodological school.',
    'If gaps exist and are resolved through weak hadith or implicit maslaha, the textual-completeness claim is false and the Hanbali method conceals rationalist reasoning under textual clothing. If genuinely no gaps arise, the methodological restriction is justified by the actual completeness of sources. Either way, the answer determines whether the constraint is justified coordination or masked extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_completeness_vs_novel_cases, empirical, 'Whether textual sources are materially complete or whether the restriction creates hidden expansions through weak hadith.').

omega_variable(
    authentication_standards_drift,
    'Have the standards for what counts as ''authenticated hadith'' (sahih) shifted over time, and if so, do those shifts represent genuine refinement of authentication or gradual rationalist expansion disguised as textual stringency?',
    'Genealogy of hadith authentication methodology from early period to contemporary jurisprudence; analysis of which hadith were classified as weak versus authentic across different scholarly generations; examination of whether authentication standards have expanded to admit hadith that earlier scholars rejected.',
    'If standards have shifted to accommodate more hadith as authenticated, the Hanbali restriction on qiyas is effectively replaced by discretionary authentication decisions—the gatekeeping moves from explicit analogical reasoning to implicit text selection. This would indicate that the constraint''s restrictiveness is partially performative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authentication_standards_drift, empirical, 'Whether hadith authentication standards are fixed or whether they drift to accommodate juristic needs.').

omega_variable(
    bid_a_definition_scope,
    'What counts as bid''a (innovation) under the Hanbali reading, and how is that definition enforced? Are all juristic tools not explicitly authorized in texts classified as bid''a, or does the classification depend on institutional judgment?',
    'Compilation of instances where Hanbali jurisprudents declared a practice or method bid''a and their reasoning; comparison of which practices one generation classified as bid''a and a later generation accepted as legitimate; analysis of how the definition expanded or contracted over institutional history.',
    'If bid''a classification is itself subject to interpretive drift and institutional judgment, the Hanbali restriction is less a fixed rule than a framework for institutional gatekeeping. The suppression and enforcement apparatus would be maintained through continuous redefining of what violates textual fidelity, making the constraint''s persistence more about institutional authority than doctrinal principle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bid_a_definition_scope, conceptual, 'Whether bid''a is an objective category or an institutionally-defined one.').

omega_variable(
    customary_law_integration_pressure,
    'Do communities practicing under Hanbali jurisprudence retain distinct customary legal frameworks that operate alongside the restriction, and if so, what mechanisms allow those customs to persist despite doctrinal prohibition?',
    'Ethnographic and historical documentation of Muslim communities claiming Hanbali allegiance while maintaining customary legal practices (''urf); institutional analysis of how fatwa councils accommodate or override local practice; examination of whether weak-hadith preferences and sadd al-dhara''i calculations create loopholes for custom.',
    'If customary law persists through weak-hadith workarounds or implicit sadd al-dhara''i reasoning, the constraint''s suppression of custom is incomplete and the high suppression score reflects enforcement effort rather than actual closure of alternatives. The resistance score would be supported by documented persistence of suppressed practices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_integration_pressure, empirical, 'Whether customary practice genuinely closes off or persists through institutional accommodation.').

omega_variable(
    rationalist_lineage_identity_lock,
    'For Hanafi and other rationalist jurisprudents, is the commitment to qiyas and ra''y a methodological choice they could abandon, or has it become fused with their professional and intellectual identity such that exit from the Hanbali frame would require abandoning their scholarly self-conception?',
    'Historical and biographical analysis of whether rationalist jurists ever adopt Hanbali methods or vice versa; examination of the language rationalist scholars use to defend their methods (are they argued as best approaches or as expressions of who they are); study of institutional incentive structures that reward methodological specialization and penalize switching.',
    'If the commitment is purely methodological, exit is possible through re-education. If it is identity-fused (a scholar is a ''Hanafi'' or a ''rationalist'' rather than choosing methods), the identity_locked exit_options classification is justified and the constraint''s suppression operates at a deeper level—not just preventing rationalist derivation but preventing the intellectual reorientation that would make the Hanbali frame seem viable. This would support higher effective suppression than the structural score alone suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rationalist_lineage_identity_lock, empirical, 'Whether rationalist commitment is methodological choice or fused identity.').

omega_variable(
    sibling_reading_institutional_coexistence,
    'Can the four readings (Hanbali, Hanafi, Maliki, Shafi''i) genuinely coexist as legitimate institutional methodologies within contemporary Islamic jurisprudence, or has Hanbali gatekeeping effectively reduced the others to minority positions with declining institutional resources and scholarly recruitment?',
    'Institutional mapping of madrasas, fatwa councils, and scholarly lineages claiming each methodology; analysis of recruitment patterns, publication output, political support, and fatwa-issuing capacity for each school; historical timeline of whether institutional parity has eroded over the interval.',
    'If Hanbali institutional dominance has marginalized the other schools, the sibling readings are nominally present but structurally foreclosed by resource concentration. The constraint would function as a snare on the minor schools rather than a tangled rope of coordinate methodologies. If parity persists, the readings genuinely coexist despite doctrinal claims of superiority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_institutional_coexistence, empirical, 'Whether sibling methodologies retain institutional parity or are marginalized.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanbali_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__hanbali_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(usul_tr_t8, usul_al_fiqh_method__hanbali_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(usul_tr_t16, usul_al_fiqh_method__hanbali_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(usul_tr_t24, usul_al_fiqh_method__hanbali_reading, theater_ratio, 24, 0.39).
narrative_ontology:measurement(usul_tr_t32, usul_al_fiqh_method__hanbali_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement(usul_tr_t40, usul_al_fiqh_method__hanbali_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement(usul_tr_t50, usul_al_fiqh_method__hanbali_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(usul_be_t8, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(usul_be_t16, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(usul_be_t24, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(usul_be_t32, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement(usul_be_t40, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(usul_be_t50, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement(usul_su_t8, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(usul_su_t16, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 16, 0.67).
narrative_ontology:measurement(usul_su_t24, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(usul_su_t32, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 32, 0.71).
narrative_ontology:measurement(usul_su_t40, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement(usul_su_t50, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 50, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanbali_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(usul_al_fiqh_method__hanbali_reading, 0.12).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__shafii_reading).

% DUAL FORMULATION NOTE:
% The kernel usul_al_fiqh_method decomposes into four structurally distinct constraint stories, one per reading. The Hanbali reading (this story) stands in a constraint family with the Hanafi, Maliki, and Shafi'i readings, each representing a different source hierarchy, beneficiary structure, and institutional gatekeeping apparatus. Decomposition is necessary because the ε values diverge sharply across readings: the Hanbali reading instantiates high textual extractiveness (ε≈0.68) and high suppression because rationalist expansion is actively blocked; the Hanafi reading would instantiate lower extractiveness because qiyas is legitimately expansive; the Maliki and Shafi'i readings sit at intermediate extraction levels with different victim sets. The readings are not different observations of one constraint; they are different constraints arising from different instantiations of the contested kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
