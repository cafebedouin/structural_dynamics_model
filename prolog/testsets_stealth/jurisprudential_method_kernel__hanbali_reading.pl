% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__hanbali_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: jurisprudential_method_kernel__hanbali_reading
 *   human_readable: Hanbali Textualist-Exclusivist Source Rule (Qur'an/Hadith/Companion Opinions Only; Qiyas and Istihsan Condemned as Bid'ah)
 *   domain: legal/institutional/religious
 *
 * SUMMARY:
 *   Within the Hanbali school's methodological rule, binding law derives only
 *   from the literal text of the Qur'an, authenticated Hadith, the opinions
 *   of the Prophet's Companions, and consensus that is genuinely unanimous;
 *   extending law by analogical reasoning (qiyas) or juristic preference
 *   (istihsan) is condemned as bid'ah — innovation that corrupts the revealed
 *   kernel. The rule solves a real coordination problem: it anchors
 *   derivation in a fixed corpus and bars each judge from extending law by
 *   private reasoning. It also carries a sharp asymmetry: rationalist
 *   jurists' methods are condemned rather than engaged, and communities
 *   governed by custom lose legal force for arrangements the text does not
 *   attest, while the textualist scholar class holds exclusive certification
 *   of valid derivation. The claim and the metrics are independent authored
 *   facts: the claim (tangled_rope) states the structure I believe true —
 *   genuine coordination carrying asymmetric extraction under active
 *   enforcement; the metrics describe the rule's operation at interval end.
 *
 * KEY AGENTS:
 *   - senior_hanbali_authorities: agenda-setter (institutional / identity_locked) — administer the method boundary, certify valid derivation, condemn innovation
 *   - textualist_scholars: primary beneficiary (organized / identity_locked) — careers and authority ride on text-exclusive certification
 *   - hadith_specialists: secondary beneficiary (organized / mobile) — their corpus becomes the law's near-exclusive foundation
 *   - rationalist_jurists: primary target (organized / constrained) — qiyas and istihsan practitioners condemned as innovators
 *   - customary_practice_communities: target and excluded voice (powerless / trapped) — custom stripped of force with no seat in the councils
 *   - comparative_usul_scholars: analytical observer (analytical / analytical) — maps the structure, collects nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, 0.82).
domain_priors:suppression_score(jurisprudential_method_kernel__hanbali_reading, 0.78).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanbali_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanbali_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanbali_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanbali_reading, "Hanbali Textualist-Exclusivist Source Rule (Qur'an/Hadith/Companion Opinions Only; Qiyas and Istihsan Condemned as Bid'ah)").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanbali_reading, "legal/institutional/religious").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanbali_reading, '22fd538e-1bcf-4ca8-b5b3-f44efe3b68f9').
narrative_ontology:cs_kernel_codification('22fd538e-1bcf-4ca8-b5b3-f44efe3b68f9', fixed_text).
narrative_ontology:cs_authority_grounding('22fd538e-1bcf-4ca8-b5b3-f44efe3b68f9', lineage).
narrative_ontology:cs_reading_relation('22fd538e-1bcf-4ca8-b5b3-f44efe3b68f9', jurisprudential_method_kernel__hanafi_reading, forecloses).
narrative_ontology:cs_reading_relation('22fd538e-1bcf-4ca8-b5b3-f44efe3b68f9', jurisprudential_method_kernel__maliki_reading, forecloses).
narrative_ontology:cs_reading_relation('22fd538e-1bcf-4ca8-b5b3-f44efe3b68f9', jurisprudential_method_kernel__shafii_reading, forecloses).
narrative_ontology:cs_axiom('22fd538e-1bcf-4ca8-b5b3-f44efe3b68f9', foundational, exclusive_textual_derivation).
narrative_ontology:cs_axiom_status(exclusive_textual_derivation, holdable).
narrative_ontology:cs_axiom_grounding('22fd538e-1bcf-4ca8-b5b3-f44efe3b68f9', exclusive_textual_derivation, theological).
narrative_ontology:cs_axiom('22fd538e-1bcf-4ca8-b5b3-f44efe3b68f9', foundational, analogy_is_kernel_corrupting_innovation).
narrative_ontology:cs_axiom_status(analogy_is_kernel_corrupting_innovation, holdable).
narrative_ontology:cs_axiom_grounding('22fd538e-1bcf-4ca8-b5b3-f44efe3b68f9', analogy_is_kernel_corrupting_innovation, theological).
narrative_ontology:cs_axiom('22fd538e-1bcf-4ca8-b5b3-f44efe3b68f9', secondary, unanimous_ijma_exclusivity).
narrative_ontology:cs_axiom_status(unanimous_ijma_exclusivity, holdable).
narrative_ontology:cs_axiom_grounding('22fd538e-1bcf-4ca8-b5b3-f44efe3b68f9', unanimous_ijma_exclusivity, conventional).
narrative_ontology:cs_reference_frame('22fd538e-1bcf-4ca8-b5b3-f44efe3b68f9', salaf_textual_fidelity_state).
narrative_ontology:cs_drift_state('22fd538e-1bcf-4ca8-b5b3-f44efe3b68f9', post_formative_school_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('22fd538e-1bcf-4ca8-b5b3-f44efe3b68f9', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, senior_hanbali_authorities).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, textualist_scholars).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanbali_reading, hadith_specialists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanbali_reading, customary_practice_communities).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanbali_reading, companion_authority_doctrine).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanbali_reading, innovation_condemnation_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the school's senior teaching chairs and mufti appointments; certify which derivations count as valid law, publicly condemn innovation, and decide which Companion reports and which claimed consensuses are admitted. Their standing is constituted by the exclusivity rule they administer: relaxing it would dissolve the distinction that makes their certification worth collecting. Exit would mean renouncing the method that constitutes their authority.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, senior_hanbali_authorities, agenda_setter,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanbali_reading, senior_hanbali_authorities, beneficiary).

% Build careers on mastery of the revealed corpus, Companion reports, and the school's condemnation doctrine; teaching posts, fatwa authority, and court appointment flow through certification in the method. Their accumulated capital — memorized text, report criticism, fidelity reputation — holds its value only while text-exclusive derivation governs; adopting analogical method would force them to compete on rationalist ground where that capital is worth less.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, textualist_scholars, beneficiary,
    organized, biographical, identity_locked, regional).

% Transmit and authenticate prophetic reports. The rule makes their corpus the near-exclusive foundation of law, raising demand for their authentication work and their standing in every methodological dispute. Their skills are portable across schools, so leaving is comparatively cheap — their position is comfortable but not captive.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, hadith_specialists, beneficiary,
    organized, biographical, mobile, continental).

% Trained in analogical reasoning and juristic preference — the methods the rule condemns as corrupting innovation. Within Hanbali-governed institutions their reasoning is inadmissible: they face condemnation, exclusion from office, or the labor of relabeling their inferences as textual implication. Rival schools admit their method, but leaving costs standing, students, and access to courts in Hanbali jurisdictions.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, rationalist_jurists, payer,
    organized, biographical, constrained, continental).

% Live under norms of custom rather than text — marriage forms, commercial practice, local procedure. When the rule denies legal force to what the text does not attest, their settled arrangements become contestable in court, and no seat exists in the methodological councils where their practice could argue for itself. Individually they have no exit from the legal order that adjudicates them.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, customary_practice_communities, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanbali_reading, customary_practice_communities, excluded).

% Historians and theorists of Islamic legal method who map how the four readings allocate validity among sources. They take no side in the contest, administer nothing, and collect nothing from the rule's operation; their seat is the outside view that makes the structure visible.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanbali_reading, comparative_usul_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__hanbali_reading, textualist_scholars).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__hanbali_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes one textually anchored source hierarchy for a dispersed community of jurists: derivation runs from the revealed corpus and Companion attestation, and no judge may extend binding law by private reasoning — every valid ruling traces to the same fixed kernel.
% TRANSFER_FUNCTION: Moves derivation authority and legal-legitimacy standing from rationalist jurists (whose methods are condemned as innovation) and from customary practice (stripped of binding force unless textually attested) to the textualist scholar class, which holds exclusive certification of valid law.
% ABSENT_VOICES: Customary-practice communities and rationalist jurists have no seat in the methodological councils that define validity; their objection enters the record only as the 'innovation' being condemned. Laypeople whose customary arrangements are adjudicated under the rule are represented by no one in the rule's own terms.
% DISAPPEARANCE_RATIONALE: If the exclusivity rule vanished overnight, derivation in Hanbali domains would reorganize within a generation: rationalist jurists' work would become admissible, unattested custom would regain argumentative force in court, and the textualist scholar class's certification monopoly would dissolve into one method among several. Whether that rearrangement is corruption or correction is precisely the kernel contest; that the arrangements would rearrange is not disputed by any party.
% FOUNDING_PROBLEM: After the Prophet's death the community faced a permanent stream of cases with no direct textual ruling, and rival authorities — Companion opinions, regional practice, personal reasoning — produced divergent law. This reading was built to hold binding law to what revelation and the first generation actually said, against the perceived corruption of extending it by human reasoning.
% FOUNDING_PROBLEM_CORROBORATION: Rival-school jurists and rationalist theologians attest the founding problem is live — every madhhab exists to answer the textless-case problem — while disputing this reading's remedy; academic historians of usul al-fiqh, outside every beneficiary set, document the formative divergence the rule responds to. No party disputes that the problem exists; the contest is over the solution.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanbali_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanbali_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanbali_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__hanbali_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__hanbali_reading, 0.82, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__hanbali_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__hanbali_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__hanbali_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.82 at interval end) because the rule does not merely tax or obligate — it forecloses entire intellectual methods and strips customary law of binding force, transferring derivation authority to a single class. Suppression is high (0.78) because persistence depends on actively condemning innovation and institutionally excluding rival methods, not on voluntary preference; the enforcement machinery (public condemnation, certification control, exclusion from office) must keep the alternatives down. Theater is moderate (0.30): the school's practice developed text-implication reasoning (dalalat al-nass) and similar workarounds that perform fidelity to the no-analogy rule while doing structurally analogous work — the formal condemnation persists while the operative method drifts. Accessibility collapse is moderate (0.48): within the reading's framework, accepting the textualist premise forecloses the alternatives almost completely, but across the legal landscape the rival schools keep analogical method live and reachable. Resistance is moderate-high (0.58): rationalist jurist establishments, rationalist theology, and the sheer practical pressure of textless cases generated sustained contest and the workarounds themselves. All three temporal series share one grid (900–1360); extraction and enforcement intensity ratchet upward together as the school institutionalizes, while theater rises in parallel as practice drifts from the formal rule.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat the rule is fidelity: the kernel is fixed, and every departure is corruption to be condemned. From the rationalist jurist seat the same rule is methodological excommunication: capacities trained over a lifetime are ruled inadmissible by a criterion they reject, and their only exits cost standing and jurisdiction. From the customary-practice seat it is silent dispossession: settled arrangements become contestable without anyone ever arguing the case against custom. The engine computes these per-seat classifications from power, exit, and directionality; the divergence is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary seats (senior authorities, textualist scholars, hadith specialists) sit near the beneficiary end: the rule subsidizes precisely the capital they hold. Rationalist jurists sit near the target end — victim declaration plus constrained exit (rival schools exist but leaving costs standing, students, and court access) amplifies their effective extraction. Customary-practice communities sit at the full-target end: victim declaration plus trapped exit, with the additional structural fact that they hold no voice in the councils that define validity. Scope runs regional (school core) to continental (rival-school labor market); verifying 'genuine unanimity' and textual fidelity is hard at that scale, which the engine's scope modifier registers. No directionality overrides are needed — the beneficiary/victim declarations plus exit atoms produce the correct d for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — cases with no textual ruling — is permanently live, so there is no mandatrophy to resolve and none is declared. The tangled_rope classification prevents both adjacent mislabels: reading the rule as pure coordination would erase the foreclosed methods and the stripped custom; reading it as pure extraction would erase the genuine coordination function (a fixed textual anchor, a bar on arbitrary judicial extension) that even the rule's opponents implicitly rely on when they attack unlimited reasoning. The receipt surface sharpens this: the gains demonstrably accrue to the textualist scholar seat, and fixing is prohibitive for the only actors who could fix it, because relaxing the rule dissolves the distinction that constitutes their authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the jurisprudential_method_kernel; what would each sibling reading change structurally, and where exactly is the disagreement located?',
    'Comparative analysis of the four readings'' source hierarchies: the disagreement is located entirely in the validity and rank of non-textual sources (qiyas, istihsan, living Medinan practice) — not in the authority of Qur''an and Hadith, which every reading shares.',
    'If the shared textual core is separable from the exclusivity axiom, the kernel could be modeled as a common coordination layer with four distinct superstructures; if not, each reading is a fully distinct constraint with no shared substructure and no common epsilon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Location and consequence of the four-reading contest over the jurisprudential method kernel.').

omega_variable(
    analogy_textual_implication_boundary,
    'Where does condemned analogy (qiyas) end and sanctioned textual implication (dalalat al-nass) begin — and is that boundary stable enough for the rule to operate as stated?',
    'Systematic coding of rulings the school condemns as analogy against rulings it validates as textual implication, extracting the operative criterion in each case.',
    'If the boundary tracks preferred outcomes rather than method, the no-analogy axiom functions as a labeling regime over reasoning the school cannot do without — raising theater_ratio and generating reclassification pressure within the school''s self-presentation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(analogy_textual_implication_boundary, empirical, 'Stability of the boundary between condemned analogy and sanctioned textual implication.').

omega_variable(
    unanimous_ijma_attainability,
    'After the Companion generation, is genuinely unanimous consensus ever attainable — and if not, is the rule''s consensus channel structurally inert?',
    'Inventory of post-Companion consensus claims together with the dissents recorded against each; if every claimed unanimity has a documented objector, the channel is inert.',
    'If inert, live derivation collapses onto text and Companion opinions alone; the rule becomes stricter than any working legal system can bear, and the drift toward reasoning workarounds is structurally forced rather than corruption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unanimous_ijma_attainability, empirical, 'Whether the unanimous-consensus channel is attainable or structurally inert.').

omega_variable(
    companion_opinion_canon_boundary,
    'Which opinions count as ''Companion opinions'' is itself settled by scholarly weighing — the very activity the rule restricts; is the rule''s second source stable, or does it reintroduce the condemned method at the foundation?',
    'Trace how the school''s canon of Companion opinions was compiled and contested; code whether admission criteria are textual (attestation chains) or methodological (plausibility weighing).',
    'If methodological, the rule is reflexively unstable — its foundation requires what it condemns — and the exclusivity axiom''s authority is performatively maintained rather than structurally operative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(companion_opinion_canon_boundary, conceptual, 'Reflexive instability of the Companion-opinion source under the rule''s own restrictions.').

omega_variable(
    fidelity_vs_scholarly_rent,
    'How much of the exclusivity rule''s cost to non-textualists is the inherent price of textual fidelity, and how much is position rent collected by the scholar class whose capital the rule protects?',
    'Counterfactual comparison: model the same fidelity commitment under open certification (any method may propose a ruling, but text must warrant it) and test whether the textualist scholars'' authority and livelihood differentials persist.',
    'If the differentials persist under open certification, the asymmetry is positional rather than doctrinal and the coordination/extraction balance shifts toward the extractive pole; if they vanish, the asymmetry is the price of the coordination itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fidelity_vs_scholarly_rent, empirical, 'Doctrinal-fidelity versus positional-rent decomposition of the rule''s asymmetry.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanbali_reading, 900, 1360).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t900, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 900, 0.12).
narrative_ontology:measurement(juri_tr_t970, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 970, 0.16).
narrative_ontology:measurement(juri_tr_t1050, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 1050, 0.2).
narrative_ontology:measurement(juri_tr_t1150, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 1150, 0.24).
narrative_ontology:measurement(juri_tr_t1260, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 1260, 0.27).
narrative_ontology:measurement(juri_tr_t1360, jurisprudential_method_kernel__hanbali_reading, theater_ratio, 1360, 0.3).

% Extraction over time
narrative_ontology:measurement(juri_be_t900, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 900, 0.55).
narrative_ontology:measurement(juri_be_t970, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 970, 0.62).
narrative_ontology:measurement(juri_be_t1050, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 1050, 0.68).
narrative_ontology:measurement(juri_be_t1150, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 1150, 0.74).
narrative_ontology:measurement(juri_be_t1260, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 1260, 0.78).
narrative_ontology:measurement(juri_be_t1360, jurisprudential_method_kernel__hanbali_reading, base_extractiveness, 1360, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t900, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 900, 0.55).
narrative_ontology:measurement(juri_su_t970, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 970, 0.61).
narrative_ontology:measurement(juri_su_t1050, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 1050, 0.66).
narrative_ontology:measurement(juri_su_t1150, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 1150, 0.71).
narrative_ontology:measurement(juri_su_t1260, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 1260, 0.75).
narrative_ontology:measurement(juri_su_t1360, jurisprudential_method_kernel__hanbali_reading, suppression_requirement, 1360, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanbali_reading, identity_coordination).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__hanafi_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanbali_reading, jurisprudential_method_kernel__shafii_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Islamic jurisprudential method' covers four structurally distinct source-hierarchy claims, decomposed per the epsilon-invariance principle into four linked stories. This story instantiates the Hanbali (textualist-exclusivist) reading; its epsilon is high because that arrangement forecloses rival methods and strips unattested custom of force. The sibling stories author their own epsilon values for their own arrangements (the Hanafi and Shafi'i readings legitimize qiyas and so extract from a different victim set; the Maliki reading legitimizes living practice). The upstream shared element — the authority of Qur'an and Hadith — is common to all four and is not separately modeled here; the exclusivity axiom layered above it is what this story measures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
