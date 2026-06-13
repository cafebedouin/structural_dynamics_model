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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: usul_al_fiqh_method__hanbali_reading
 *   human_readable: Hanbali Usul al-Fiqh Method: Textual Maximalism and Innovation Blocking
 *   domain: religious/legal/epistemological
 *
 * SUMMARY:
 *   The Hanbali school of Islamic jurisprudence instantiates one reading of
 *   the contested usul al-fiqh (foundational jurisprudence) kernel: the claim
 *   that Islamic law is maximally constrained by textual sources (Quran and
 *   authenticated hadith) and must minimize reliance on qiyas (analogical
 *   reasoning), exclude weak hadith in favor of preserved text, and
 *   preemptively block innovations (bid'a) through the sadd al-dhara'i
 *   principle. This reading is a lived institutional constraint enforced
 *   through educational curricula, fatwa authority, and orthodox gatekeeping
 *   across Hanbali communities. The constraint coordinates a unified
 *   textualist methodology while extracting substantial cost from jurists and
 *   communities seeking adaptive legal development. Sibling readings—Hanafi
 *   expansive qiyas, Maliki customary integration, Shafi'i systematized
 *   hadith authentication—represent alternative institutional framings of the
 *   same kernel; they coexist as live madhab positions but the Hanbali method
 *   actively suppresses their legitimacy within its own institutional scope.
 *
 * KEY AGENTS:
 *   - Hanbali school institutional authorities: set and enforce the textualist boundary; identity-locked commitment to the method
 *   - Textualist gatekeepers and reform movements: benefit from doctrinal control; collect authority and legitimacy from fidelity enforcement
 *   - Rationalist jurists (Hanafi, Maliki, Shafi'i schools): constrained from deploying expansive analogical reasoning; bear the cost of secondary institutional status
 *   - Communities seeking customary legal integration: blocked from adapting law to regional practice; bear cost of inflexibility
 *   - Adaptive legal innovation advocates: powerless to develop jurisprudence for novel domains; identity-locked to Islamic law while structurally prevented from developing it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanbali_reading, 0.68).
domain_priors:suppression_score(usul_al_fiqh_method__hanbali_reading, 0.71).
domain_priors:theater_ratio(usul_al_fiqh_method__hanbali_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanbali_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanbali_reading, "Hanbali Usul al-Fiqh Method: Textual Maximalism and Innovation Blocking").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanbali_reading, "religious/legal/epistemological").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanbali_reading, '26740e49-e4b9-41fb-95eb-0398fa796d14').
narrative_ontology:cs_kernel_codification('26740e49-e4b9-41fb-95eb-0398fa796d14', fixed_text).
narrative_ontology:cs_authority_grounding('26740e49-e4b9-41fb-95eb-0398fa796d14', lineage).
narrative_ontology:cs_interpretation_layer_present('26740e49-e4b9-41fb-95eb-0398fa796d14').
narrative_ontology:cs_reading_relation('26740e49-e4b9-41fb-95eb-0398fa796d14', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('26740e49-e4b9-41fb-95eb-0398fa796d14', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('26740e49-e4b9-41fb-95eb-0398fa796d14', usul_al_fiqh_method__shafii_reading, coexists_with).
narrative_ontology:cs_axiom('26740e49-e4b9-41fb-95eb-0398fa796d14', foundational, textual_sources_self_sufficing).
narrative_ontology:cs_axiom_status(textual_sources_self_sufficing, holdable).
narrative_ontology:cs_axiom_grounding('26740e49-e4b9-41fb-95eb-0398fa796d14', textual_sources_self_sufficing, deontological).
narrative_ontology:cs_axiom('26740e49-e4b9-41fb-95eb-0398fa796d14', foundational, innovation_blocking_preemptive).
narrative_ontology:cs_axiom_status(innovation_blocking_preemptive, holdable).
narrative_ontology:cs_axiom_grounding('26740e49-e4b9-41fb-95eb-0398fa796d14', innovation_blocking_preemptive, empirically_contingent).
narrative_ontology:cs_reference_frame('26740e49-e4b9-41fb-95eb-0398fa796d14', textual_sufficiency_doctrine).
narrative_ontology:cs_drift_state('26740e49-e4b9-41fb-95eb-0398fa796d14', contemporary_novel_legal_questions_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('26740e49-e4b9-41fb-95eb-0398fa796d14', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, textualist_scholars).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, orthodox_gatekeepers).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, textual_fidelity_doctrine).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, customary_practice_developers).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, adaptive_legal_innovation).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, textualist_reform_movements).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, hanafi_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, maliki_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, shafii_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, customary_practice_communities).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, contemporary_legal_innovation_advocates).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanbali_reading, bid_a_prohibition).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanbali_reading, textual_sufficiency_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Authoritatively interpret Islamic law through maximalist adherence to Quranic text and authenticated hadith. Set the jurisprudential boundaries by defining which hadith grades qualify, when qiyas is permissible, and which innovations (bid'a) are blocked. Their professional and doctrinal identity is constituted through commitment to this interpretive method. They enforce the boundaries through fatwa issuance, educational transmission, and institutional gatekeeping in Hanbali communities and wherever Hanbali methodology is institutionally dominant.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, hanbali_school_jurists, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Contemporary fundamentalist and reform movements advancing textual fidelity and opposition to Western-influenced legal adaptation benefit from the Hanbali method's institutional legitimacy. They leverage textualist authority to resist customary legal integration and rationalist jurisprudential development. They collect organizational authority and ideological legitimacy from the constraint's enforcement.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, textualist_reform_movements, beneficiary,
    institutional, civilizational, arbitrage, global).

% Hanafi school jurists whose methodology emphasizes expansive qiyas and ra'y encounter systematic delegitimation in contexts where Hanbali textualism is institutionally dominant. Their jurisprudential authority is subordinated; their methodological approaches are characterized as rationalism or excessive reasoning. They maintain parallel institutional structures (Hanafi madrasas, fatwa networks) but lack institutional parity in contexts dominated by Hanbali orthodoxy.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, hanafi_jurists, payer,
    institutional, civilizational, constrained, global).

% Maliki school jurists whose methodology integrates local customary practice ('urf) and public interest (maslaha mursala) face institutional resistance from Hanbali-dominant contexts. Their method of balancing textual source with customary integration is characterized as bid'a or innovation without warrant. They maintain institutional presence primarily in regions with historical Maliki establishment but experience subordination in institutions where Hanbali methodology is canonical.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, maliki_jurists, payer,
    institutional, civilizational, constrained, global).

% Shafi'i school jurists whose methodology systematizes hadith authentication and permits qiyas encounter institutional skepticism in Hanbali-dominant contexts regarding their authentication standards and analogical reasoning. Their sophisticated meta-discipline of usul al-fiqh is seen as overly rationalized. They maintain strong institutional presence in Southeast Asia and some Middle Eastern regions but experience secondary status in Hanbali-dominant institutions.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, shafii_jurists, payer,
    institutional, civilizational, constrained, global).

% Communities seeking to integrate established customary legal practice (land tenure, inheritance variation, dispute resolution) with Islamic law encounter the blocking mechanism (sadd al-dhara'i) that preemptively excludes such integration to prevent innovation. They must either suppress their customary practice to comply with textualist methodology, or conduct their practice in institutional hiding. Their adaptive legal capacity is constrained; their regional variation is subordinated to textual uniformity.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, customary_practice_communities, payer,
    organized, biographical, constrained, regional).

% Jurists and communities seeking to develop legal responses to new technological, medical, and governance questions absent from early Islamic texts (bioethics, artificial intelligence, climate justice) are structurally blocked by the textualist method. They face the bid'a charge when proposing developmental reasoning. They cannot exit Islamic jurisprudential identity without abandoning their professional and intellectual commitments. They must either suppress their development, hide it from orthodox scrutiny, or wage intellectual combat against the textualist gatekeepers.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, contemporary_legal_innovation_advocates, payer,
    powerless, biographical, identity_locked, regional).

% Hanafi, Maliki, and Shafi'i institutional authorities would argue for the legitimacy and necessity of their expanded methodologies and their diverse approaches to balancing text with reasoning. They are structurally excluded from the Hanbali framework's internal deliberation and canonical authority. Their alternative readings remain live in other institutional networks and regions but do not shape Hanbali jurisprudence from within. They are trapped because exit from the Islamic jurisprudential ecology itself is not a realistic option.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, sibling_madhab_authorities, excluded,
    institutional, civilizational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__hanbali_reading, textualist_reform_movements).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__hanbali_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified, consistent, textually-grounded legal methodology across Hanbali communities worldwide: a single standard for determining what counts as valid evidence (Quranic text, hadith grade thresholds), what analogies are permitted (only where text is demonstrably silent), and what counts as legitimate legal development (none that contradicts or supplements the text via mechanisms other than textual analogy). Solves the coordination problem of preventing institutional fragmentation and unauthorized innovation through idiosyncratic reasoning or ungrounded custom incursion.
% TRANSFER_FUNCTION: Transfers interpretive authority from distributed jurists and communities to the centralized textualist framework and its institutional guardians. Moves adaptive legal capacity from innovative jurists and customary practitioners to the doctrine of textual fidelity. Moves legitimacy-claiming power from rationalist reasoning and customary integration to textual restriction. The constraint extracts the cost of legal rigidity from those seeking adaptive development and concentrates the benefit of doctrinal control in textualist gatekeepers and reform movements using textualism as legitimacy.
% ABSENT_VOICES: Hanafi, Maliki, and Shafi'i institutional authorities would argue that their alternative methodologies have functionally prevented the fragmentation the textualist method presumes to prevent, and that textual restriction unnecessarily sacrifices adaptive capacity without corresponding benefit. Communities and jurists seeking legal development in novel domains are absent—they have been excluded from the table where methodology is enforced and their proposed innovations are preemptively blocked.
% DISAPPEARANCE_RATIONALE: If the Hanbali textualist methodology and its enforcement mechanisms disappeared overnight, the jurisprudential ecosystem would reorganize: adaptive legal reasoning would resume in Hanbali communities; customary practice would reintegrate in regions where Hanbali methodology had suppressed it; contemporary legal questions would be addressed through expanded analogical reasoning and public-interest consideration; the pace of jurisprudential development would accelerate. Hanbali institutional authority would face competition from alternative methodologies; the monopoly on legitimacy that textualism provides to orthodox gatekeepers would fragment.
% FOUNDING_PROBLEM: Early Islamic jurisprudence risked fragmentation and unauthorized innovation (bid'a) as individual jurists deployed weak hadith and expansive analogical reasoning beyond the textual warrant of the Quran and authenticated Sunna. The textualist method was developed to prevent deviation and preserve the integral authority of the textual sources as the sole legitimate grounds of Islamic law.
% FOUNDING_PROBLEM_CORROBORATION: Hanbali institutional authorities attest the problem is perpetually live: rationalist reasoning and speculative analogy continue to threaten textual integrity. Hanafi, Maliki, and Shafi'i schools attest the problem was substantially addressed by their own methodologies, which establish textual sources as foundation while permitting supplementary reasoning within defined bounds. Independent historians of Islamic law (Wael Hallaq, Christopher Melchert, others outside the madhab institutional frameworks) note that the early risk of fragmentary jurisprudence has been institutionalized through the development of rival but coherent madhab systems, each with internal consistency checks and pedagogical standardization. The founding problem appears to be less an ongoing acute threat and more a historical condition that institutional madhab development has addressed through diverse mechanisms—not all of which require the textualist restriction the Hanbali method enforces.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanbali_reading, world_rearranges).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanbali_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanbali_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(usul_al_fiqh_method__hanbali_reading, 'none', 1).

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
 *   Extractiveness (0.68 at interval end) is substantial because the constraint concentrates interpretive authority and blocks adaptive capacity without clear textual warrant for the blocking itself. Suppression (0.71) is higher than extractiveness because the constraint's persistence depends on active enforcement of the bid'a charge and the exclusion of competing madhab methodologies from institutional legitimacy. Enforcement must continuously exclude Hanafi qiyas, Maliki maslaha, and Shafi'i systematization to maintain textualist primacy. Theater (0.28) is moderate-low: the coordination function is real (unified textual standard), but an increasing share of the enforcement effort is devoted to suppressing adaptive reasoning rather than preserving textual integrity itself. The measurement series show rising theater_ratio and suppression_requirement over the interval, suggesting that as the founding coordination problem has become institutionalized and less acute, the constraint's functional burden has shifted from coordination toward suppression of alternatives.
 *
 * PERSPECTIVAL GAP:
 *   The Hanbali institutional seat experiences the constraint as protective coordination: preservation of textual fidelity against rationalist dilution. The payer seats (rationalist jurists, customary developers, innovation advocates) experience the same constraint as enforced exclusion: suppression of their legitimate methodological alternatives. The engine computes different types from these positions because the structural relationships differ: the agenda-setter collects authority and exercises control (beneficiary directionality, d near 0.0); the payers bear the cost of suppression and constrained reasoning (target directionality, d near 1.0). This divergence is built into the structural data and should emerge from the engine's per-seat computation.
 *
 * DIRECTIONALITY LOGIC:
 *   Hanbali institutional authorities sit at the agenda-setter pole (d ≈ 0.1–0.2): they set boundaries, enforce methodology, collect the authority that textualist control grants. Rationalist jurists (Hanafi, Maliki, Shafi'i) sit at moderate target (d ≈ 0.65–0.75): their methodologies are constrained, their institutional standing is subordinated, their professional autonomy is reduced, but they retain institutional power and can maintain parallel schools. Customary practice developers sit at high target (d ≈ 0.75–0.85): they face active blocking, lack institutional power to resist, and encounter identity-locking (accepting the charge of bid'a or abandoning Islamic jurisprudential legitimacy). Adaptive innovation advocates sit at maximum target (d ≈ 0.90–1.0): powerless, facing preemptive blocking, unable to develop lawful responses to new domains without the constraint suppressing their output. The directionality structure reflects the asymmetry: textualist gatekeepers benefit, rationalist schools constrain, communities and innovations are targeted.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint is classified as tangled_rope (coordinated methodology + asymmetric extraction + active enforcement) rather than as a false natural law or pure snare. The founding coordination problem—preventing fragmentary jurisprudence through textual standards—was genuine and remains partially live (textual integrity is valuable). However, the constraint's persistence also depends on suppressing alternative methodologies that other madhabs have proven functionally adequate to prevent fragmentation. The mandatrophy question is whether the textualist method still solves the founding problem (fragmentation risk) or whether it now primarily serves to concentrate interpretive authority. The evidence leans toward the latter: institutional madhab systems have long solved the fragmentation problem through diverse but systematic methods; the continued enforcement of textualist exclusivity appears more extractive than coordinating. The constraint exhibits mandatrophy: the founding problem that justified the method's emergence (fragmentation without institutional control) has been structurally superseded by the institutionalization of all madhabs, yet the constraint persists and intensifies in its enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_continued_acuity,
    'Is the founding problem—fragmentation and unauthorized innovation in jurisprudence—still acute, or has it been institutionalized away by the development of rival madhab systems with their own internal coherence?',
    'Comparative analysis of jurisprudential divergence across madhabs and within Hanbali school over time; examination of whether textual fidelity is demonstrably better preserved under the Hanbali method than under Maliki or Shafi''i methods that permit supplementary reasoning.',
    'If the problem is no longer acute (living madhab diversity shows coherence without textualist maximalism), the constraint exhibits mandatrophy: persisting after its original justification has been superseded. If the problem remains acute, the suppressive aspects are justified as coordination cost. This determines whether the constraint should be reclassified as piton (inertial theater) or remains tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_continued_acuity, empirical, 'Whether textualist methodology remains necessary to prevent jurisprudential fragmentation.').

omega_variable(
    suppression_mechanism_internalized_vs_structural,
    'Is the measured suppression (0.71) structural (institutional exclusion, gatekeeping, bid''a charges actively imposed from outside) or internalized (jurists in other madhabs have absorbed textualist primacy as ideologically legitimate)?',
    'Historical analysis of institutional dynamics in contexts where Hanbali authority is dominant versus where madhabs coexist: Do Hanafi and Maliki jurists suppress their own methodologies voluntarily, or is suppression actively maintained by Hanbali institutional gatekeeping? What would happen to Hanbali innovation rates if Hanbali jurists exited Hanbali communities without losing Islamic jurisprudential legitimacy?',
    'If internalized, the constraint''s effective suppression is higher than the structural measure—payers carry the suppression with them even after institutional exit. If structural, the suppression may be escapable through jurisdictional change. This affects exit-option classification (identity_locked vs. constrained) for jurists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalized_vs_structural, empirical, 'Whether measured suppression is maintained by external gatekeeping or has become internalized in jurists'' self-perception.').

omega_variable(
    textual_sufficiency_vs_interpretive_necessity,
    'Is the foundational axiom—that Islamic textual sources are epistemically sufficient for all legal questions—defensible against the historical reality that jurists across all madhabs employ interpretive reasoning beyond the text?',
    'Textual analysis: Can the Quran and authenticated hadith realistically be shown to address questions of modern financial technology, medical ethics, governance systems that did not exist in the seventh century? Or does the claim to textual sufficiency rest on interpreting ''textual'' to include reasoning that draws on but goes beyond explicit text?',
    'If textual sufficiency is unsustainable against novel questions, the foundational axiom is overridden by practical necessity, and the constraint becomes a formalistic blocking mechanism without substantive justification. If some form of interpretive reasoning is unavoidable, the Hanbali method''s attempt to minimize reasoning through qiyas restriction transfers the reasoning to other mechanisms (weak hadith preference, analogy by extension, implicit permissibility). The classification remains tangled_rope but the mandatrophy profile shifts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_sufficiency_vs_interpretive_necessity, conceptual, 'Whether textual sufficiency as a foundational claim can be coherently sustained.').

omega_variable(
    bid_a_charge_as_suppression_mechanism,
    'Does the sadd al-dhara''i principle (blocking innovations to prevent harm) function as a legitimate jurisprudential safeguard, or does it operate primarily as a rhetorical suppression mechanism (any adaptive reasoning labeled bid''a without requiring proof of harm)?',
    'Institutional case analysis: When bid''a charges are leveled against proposed adaptive jurisprudence, how often is concrete harm identified versus how often is the charge made preemptively? What proportion of contemporary Hanbali fatwa authority goes to developing law for novel circumstances versus blocking proposed development?',
    'If bid''a charges are preemptive and harm-unconditional, the blocking mechanism is revealed as suppressive rather than protective, and suppression (0.71) may be understated. If bid''a charges are substantively grounded in identified harms, the blocking is coordinating (preventing real risk) and the extraction is overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bid_a_charge_as_suppression_mechanism, empirical, 'Whether the bid''a blocking mechanism operates as substantive harm prevention or as suppression of adaptive reasoning.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanbali_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__hanbali_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(usul_tr_t0, observed).
narrative_ontology:measurement(usul_tr_t8, usul_al_fiqh_method__hanbali_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement_basis(usul_tr_t8, observed).
narrative_ontology:measurement(usul_tr_t16, usul_al_fiqh_method__hanbali_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement_basis(usul_tr_t16, observed).
narrative_ontology:measurement(usul_tr_t24, usul_al_fiqh_method__hanbali_reading, theater_ratio, 24, 0.24).
narrative_ontology:measurement_basis(usul_tr_t24, observed).
narrative_ontology:measurement(usul_tr_t32, usul_al_fiqh_method__hanbali_reading, theater_ratio, 32, 0.26).
narrative_ontology:measurement_basis(usul_tr_t32, observed).
narrative_ontology:measurement(usul_tr_t40, usul_al_fiqh_method__hanbali_reading, theater_ratio, 40, 0.28).
narrative_ontology:measurement_basis(usul_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement_basis(usul_be_t0, observed).
narrative_ontology:measurement(usul_be_t8, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(usul_be_t8, observed).
narrative_ontology:measurement(usul_be_t16, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 16, 0.63).
narrative_ontology:measurement_basis(usul_be_t16, observed).
narrative_ontology:measurement(usul_be_t24, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 24, 0.66).
narrative_ontology:measurement_basis(usul_be_t24, observed).
narrative_ontology:measurement(usul_be_t32, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(usul_be_t32, observed).
narrative_ontology:measurement(usul_be_t40, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(usul_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(usul_su_t0, observed).
narrative_ontology:measurement(usul_su_t8, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 8, 0.59).
narrative_ontology:measurement_basis(usul_su_t8, observed).
narrative_ontology:measurement(usul_su_t16, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 16, 0.63).
narrative_ontology:measurement_basis(usul_su_t16, observed).
narrative_ontology:measurement(usul_su_t24, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 24, 0.67).
narrative_ontology:measurement_basis(usul_su_t24, observed).
narrative_ontology:measurement(usul_su_t32, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 32, 0.69).
narrative_ontology:measurement_basis(usul_su_t32, observed).
narrative_ontology:measurement(usul_su_t40, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(usul_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanbali_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(usul_al_fiqh_method__hanbali_reading, 0.18).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method__shafii_reading).

% DUAL FORMULATION NOTE:
% The Hanbali reading is one of four constraint stories instantiating the contested usul al-fiqh kernel. All four sibling readings (Hanafi, Maliki, Shafi'i, Hanbali) decompose the historical fact of madhab diversity into structurally distinct constraint stories, each with its own epsilon-invariant ε, beneficiary/victim structure, and type. The ε values diverge because the claims differ: Hanbali maximalist textualism is substantially extractive (0.68); Hanafi expansive qiyas is less extractive (estimated 0.45–0.55); Maliki customary integration is moderately extractive (estimated 0.50–0.60); Shafi'i systematized hadith is minimally extractive (estimated 0.35–0.45). These are four different constraints, not one constraint viewed through four lenses. The sibling readings are linked via the affects_constraints array and via cs_structure.reading_relations in each story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(usul_al_fiqh_method__hanbali_reading, moderate, 0.72).
constraint_indexing:directionality_override(usul_al_fiqh_method__hanbali_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
