% ============================================================================
% CONSTRAINT STORY: usul_al_fiqh_method__hanbali_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   human_readable: Hanbali Textual-Restrictionist Reading of Usul al-Fiqh
 *   domain: religious_legal/jurisprudential
 *
 * SUMMARY:
 *   This constraint models the Hanbali reading of the shared usul al-fiqh
 *   kernel: a source-hierarchy commitment that treats Quran and authenticated
 *   hadith as maximally restrictive, confines analogical reasoning (qiyas) to
 *   genuine textual silence, prefers even weak hadith over analogical
 *   derivation, and actively blocks innovations (sadd al-dhara'i) that might
 *   open interpretive space beyond the text. This is one of four sibling
 *   readings of the same underlying kernel (usul_al_fiqh_method); the Hanafi,
 *   Maliki, and Shafi'i readings are separate constraint stories with their
 *   own epsilon values and beneficiary/victim structures, not observable
 *   variants of this one. The Hanbali reading is authored here at its own
 *   epsilon, assessed by its own lights, describing the standing arrangement
 *   (its dominant interpretive method as institutionally administered) rather
 *   than any endorsed alternative.
 *
 * KEY AGENTS:
 *   - hanbali_textualist_scholars: agenda_setter/beneficiary (institutional/identity_locked) — administers the method, gains authority from its textualist restrictiveness
 *   - traditionist_hadith_transmitters: beneficiary (organized/constrained) — their vocation is elevated by weak-hadith-over-qiyas preference
 *   - rationalist_jurists: payer (moderate/constrained) — their preferred reasoning tools are minimized
 *   - customary_practice_communities: payer (powerless/trapped) — local practice treated as innovation risk
 *   - adherents_of_local_urf: payer (powerless/trapped) — customs foreclosed absent direct textual grounding
 *   - sibling_school_jurists: excluded (organized/arbitrage) — operate under different readings of the same kernel, not part of this reading's internal deliberation
 *   - comparative_legal_historians: observer (analytical) — documents the reading without adjudicating theological correctness
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(usul_al_fiqh_method__hanbali_reading, 0.52).
domain_priors:suppression_score(usul_al_fiqh_method__hanbali_reading, 0.61).
domain_priors:theater_ratio(usul_al_fiqh_method__hanbali_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(usul_al_fiqh_method__hanbali_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(usul_al_fiqh_method__hanbali_reading, tangled_rope).
narrative_ontology:human_readable(usul_al_fiqh_method__hanbali_reading, "Hanbali Textual-Restrictionist Reading of Usul al-Fiqh").
narrative_ontology:topic_domain(usul_al_fiqh_method__hanbali_reading, "religious_legal/jurisprudential").

domain_priors:requires_active_enforcement(usul_al_fiqh_method__hanbali_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(usul_al_fiqh_method__hanbali_reading, '72a7b3ee-ebf8-4993-868d-68ceb5f24139').
narrative_ontology:cs_kernel_codification('72a7b3ee-ebf8-4993-868d-68ceb5f24139', formalized).
narrative_ontology:cs_authority_grounding('72a7b3ee-ebf8-4993-868d-68ceb5f24139', lineage).
narrative_ontology:cs_interpretation_layer_present('72a7b3ee-ebf8-4993-868d-68ceb5f24139').
narrative_ontology:cs_reading_relation('72a7b3ee-ebf8-4993-868d-68ceb5f24139', usul_al_fiqh_method__hanafi_reading, coexists_with).
narrative_ontology:cs_reading_relation('72a7b3ee-ebf8-4993-868d-68ceb5f24139', usul_al_fiqh_method__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('72a7b3ee-ebf8-4993-868d-68ceb5f24139', usul_al_fiqh_method__shafii_reading, influences).
narrative_ontology:cs_axiom('72a7b3ee-ebf8-4993-868d-68ceb5f24139', foundational, text_exhausts_legitimate_derivation_absent_true_silence).
narrative_ontology:cs_axiom_status(text_exhausts_legitimate_derivation_absent_true_silence, holdable).
narrative_ontology:cs_axiom_grounding('72a7b3ee-ebf8-4993-868d-68ceb5f24139', text_exhausts_legitimate_derivation_absent_true_silence, deontological).
narrative_ontology:cs_axiom('72a7b3ee-ebf8-4993-868d-68ceb5f24139', foundational, innovation_prevention_outweighs_interpretive_flexibility).
narrative_ontology:cs_axiom_status(innovation_prevention_outweighs_interpretive_flexibility, holdable).
narrative_ontology:cs_axiom_grounding('72a7b3ee-ebf8-4993-868d-68ceb5f24139', innovation_prevention_outweighs_interpretive_flexibility, instrumental).
narrative_ontology:cs_reference_frame('72a7b3ee-ebf8-4993-868d-68ceb5f24139', salaf_textual_primacy_framework).
narrative_ontology:cs_drift_state('72a7b3ee-ebf8-4993-868d-68ceb5f24139', post_colonial_islamic_revivalism, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('72a7b3ee-ebf8-4993-868d-68ceb5f24139', '').
narrative_ontology:cs_kernel_id(usul_al_fiqh_method__hanbali_reading, usul_al_fiqh_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, hanbali_textualist_scholars).
narrative_ontology:constraint_beneficiary(usul_al_fiqh_method__hanbali_reading, traditionist_hadith_transmitters).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, rationalist_jurists).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, customary_practice_communities).
narrative_ontology:constraint_victim(usul_al_fiqh_method__hanbali_reading, adherents_of_local_urf).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanbali_reading, textual_sufficiency_doctrine).
narrative_ontology:constraint_vindicates(usul_al_fiqh_method__hanbali_reading, sadd_al_dharai_necessity_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the interpretive method: ranks Quran and authenticated hadith above all other sources, minimizes qiyas to cases of clear textual silence, prefers weak hadith over analogical reasoning, and blocks innovations (sadd al-dhara'i) that could open avenues away from textual fidelity. Their scholarly authority and communal standing are constituted by fidelity to this method; loosening it would dissolve the basis of their distinct school identity.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, hanbali_textualist_scholars, agenda_setter,
    institutional, civilizational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(usul_al_fiqh_method__hanbali_reading, hanbali_textualist_scholars, beneficiary).

% Their vocation — collecting, authenticating, and transmitting hadith, including weak-but-textually-grounded reports — gains elevated legal weight under this method compared to schools that would discount weak hadith in favor of qiyas. The method's preference for weak hadith over analogy directly increases the relevance of their labor and status.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, traditionist_hadith_transmitters, beneficiary,
    organized, generational, constrained, regional).

% Practitioners inclined toward analogical and reasoned derivation find their preferred tools minimized to the narrow residue of cases where text is silent. Where a Hanafi or Maliki reading would validate their reasoning, this method treats it as an innovation risk to be blocked. They can migrate to another school's institutions, but within Hanbali-administered courts and educational structures their reasoning is structurally disfavored.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, rationalist_jurists, payer,
    moderate, biographical, constrained, regional).

% Local custom ('urf) and evolved community practice, which other readings integrate as evidentiary or interpretive input, are treated under sadd al-dhara'i as potential vectors of innovation (bid'a) to be closed off rather than accommodated. Communities whose lived practice diverges from strict textual precedent bear the cost of having that practice delegitimized, with little recourse within the method's own logic.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, customary_practice_communities, payer,
    powerless, generational, trapped, local).

% Ordinary practitioners whose family, commercial, or ritual customs are not explicitly text-grounded find those customs treated with suspicion or foreclosed as innovations. Their only recourse is to seek rulings from jurists of a different school, which is not always available or socially permitted where Hanbali authority is institutionally dominant.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, adherents_of_local_urf, payer,
    powerless, biographical, trapped, local).

% Hanafi, Maliki, and Shafi'i jurists hold different views on the proper scope of qiyas, istihsan, maslaha, and 'amal — they are not part of this reading's internal deliberation and their frameworks are treated by this reading as excessive openings toward innovation. They continue to operate in their own jurisdictions, largely unaffected by this reading's internal enforcement, but are foreclosed from its administrative structures.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, sibling_school_jurists, excluded,
    organized, civilizational, arbitrage, continental).

% Study the four readings comparatively, documenting how source-hierarchy commitments diverge and how each reading's beneficiary/victim structure differs from the others without adjudicating which reading is theologically correct.
narrative_ontology:constraint_stakeholder(usul_al_fiqh_method__hanbali_reading, comparative_legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(usul_al_fiqh_method__hanbali_reading, hanbali_textualist_scholars).
narrative_ontology:fixing_cost_class(usul_al_fiqh_method__hanbali_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, predictable method for deriving legal rulings that minimizes disputed interpretive discretion by anchoring nearly all derivation in directly attested text, reducing the risk that rulings drift from the earliest transmitted sources.
% TRANSFER_FUNCTION: Moves interpretive authority and legal legitimacy away from jurists relying on analogy, juristic preference, or custom, and toward scholars and transmitters whose standing rests on textual and hadith mastery; moves practical legal flexibility away from communities with strong customary practice and toward centralized textual gatekeeping.
% ABSENT_VOICES: Rationalist jurists and customary-practice communities would object that the method treats reasoned extension and lived practice as inherently suspect rather than as legitimate sources of law, but within Hanbali-administered institutions their objections are pre-empted by the sadd al-dhara'i framework itself, which classifies such objections as symptoms of the innovation it exists to block.
% DISAPPEARANCE_RATIONALE: Hanbali-administered courts, educational curricula, and fatwa institutions would need to adopt a different source hierarchy (e.g., admitting maslaha mursala or expansive qiyas), materially changing outcomes on cases involving custom, public interest, and novel circumstances; whether this counts as the world rearranging or merely one school converging toward others is disputed within the tradition itself.
% FOUNDING_PROBLEM: Early juristic disputes in the 8th-9th centuries CE saw increasing use of speculative reasoning (ra'y) and regional custom in legal rulings; the Hanbali method was built to check what its founders saw as drift away from the Prophetic model and the risk of law becoming unmoored from revealed text.
% FOUNDING_PROBLEM_CORROBORATION: Hanbali scholars themselves attest the problem (unchecked ra'y and innovation) remains live and requires the same textual discipline today. Comparative legal historians and jurists from sibling schools attest that the same historical concern was addressed differently and, in their view, adequately by other schools' more moderate mechanisms (e.g., Shafi'i's systematized hierarchy, Maliki's integration of 'amal), suggesting the founding problem was a live 9th-century methodological dispute rather than an unresolved defect requiring maximal restriction specifically.
narrative_ontology:disappearance_verdict(usul_al_fiqh_method__hanbali_reading, contested).
narrative_ontology:founding_problem_status(usul_al_fiqh_method__hanbali_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(usul_al_fiqh_method__hanbali_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(usul_al_fiqh_method__hanbali_reading, 'none', 1).
narrative_ontology:epsilon_provenance(usul_al_fiqh_method__hanbali_reading, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored at a moderate 0.52 rather than high: the method genuinely coordinates predictable legal derivation for its adherents and has a real epistemic rationale (fidelity to earliest transmitted sources), but it also structurally disadvantages rationalist and customary legal development in ways that transfer interpretive authority toward a narrower scholarly class. Suppression (0.61) reflects the active, doctrinally-grounded blocking mechanism (sadd al-dhara'i) that is specifically designed to close off alternative interpretive routes once they are identified as innovation-adjacent — this is a stronger and more deliberate suppression mechanism than the sibling readings' more permissive gates. Theater ratio is kept low (0.22) because the restrictive apparatus is substantively applied in actual legal derivation, not merely performed. Accessibility collapse is authored high (0.68) because, within Hanbali-administered institutions, once a matter is textually addressed the alternative interpretive routes are treated as functionally closed, not merely disfavored. Resistance (0.58) reflects real, sustained pushback from rationalist jurists and customary communities across centuries, which the method must continually address through further textual justification.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (Hanbali scholars), this reading is a coordination mechanism preserving fidelity to revealed sources against methodological drift. From the payer seats (rationalist jurists, customary communities), the same structure operates as an enforced narrowing of legitimate legal sources that transfers interpretive authority to a specific scholarly lineage. The engine's per-seat computation should reflect this divergence: agenda-setter and beneficiary seats likely compute nearer coordination/rope readings, while payer seats compute nearer tangled_rope or snare readings of the identical structural facts.
 *
 * DIRECTIONALITY LOGIC:
 *   Hanbali textualist scholars and hadith transmitters are structural beneficiaries: their authority, vocation, and institutional standing derive directly from the method's specific hierarchy of sources. Rationalist jurists and customary/urf-adherent communities are structural targets: the method's own logic (sadd al-dhara'i) is specifically the mechanism that curtails their preferred sources of legal reasoning. Exit options differ sharply along power lines — rationalist jurists (moderate power) retain some ability to migrate to sibling-school institutions, while customary-practice communities and urf-adherents (powerless, trapped/local) often lack meaningful access to alternative legal venues, especially where Hanbali authority is institutionally dominant (e.g., historically in the Arabian Peninsula).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (checking unmoored ra'y-based legal drift in the 8th-9th centuries) is contested as still live: Hanbali scholars maintain the risk of innovation is perennial and justifies continued maximal restriction, while sibling-school jurists and historians note the same historical concern was addressed by other schools with more moderate, still text-respecting mechanisms. This divergence is exactly what the tangled_rope classification is meant to hold open rather than resolve prematurely — the coordination function (textual fidelity, predictability) is real and should not be reduced to pure extraction, but neither should the asymmetric cost borne by rationalist and customary legal development be waved away as merely a byproduct of principled textualism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_fidelity_vs_gatekeeping_ambiguity,
    'Is the Hanbali method''s restrictiveness a genuine epistemic safeguard against interpretive drift from revealed sources, or is it functioning (at least in part) as a gatekeeping mechanism that concentrates legal authority in textualist scholars and hadith transmitters at the expense of rationalist and customary legal development?',
    'Comparative historical analysis of case outcomes across regions and eras where Hanbali courts operated alongside or instead of other schools, examining whether rulings under strict sadd al-dhara''i produced materially different (and specifically more restrictive on customary/rationalist claims) outcomes than sibling-school jurisdictions facing comparable fact patterns.',
    'If the restrictiveness tracks genuine epistemic caution with proportionate costs, the coordination function dominates and a rope-leaning reading is more defensible; if it systematically concentrates authority and forecloses customary/rationalist development beyond what epistemic caution requires, the tangled_rope/snare-leaning reading is more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_fidelity_vs_gatekeeping_ambiguity, conceptual, 'Whether Hanbali textual restrictiveness is primarily epistemic safeguard or primarily authority-concentrating gatekeeping.').

omega_variable(
    kernel_reading_disagreement_locus,
    'Where exactly does the Hanbali reading''s disagreement with sibling readings live — in the ranking of sources (text over reason), in the threshold for treating textual silence as genuine (narrow vs. broad silence-recognition), or in the risk tolerance for innovation (sadd al-dhara''i''s aggressiveness)?',
    'Textual-comparative analysis of foundational usul al-fiqh treatises across the four schools, isolating whether disputes are primarily about source hierarchy, about what counts as textual silence, or about the innovation-blocking threshold.',
    'Locating the disagreement in source hierarchy alone would suggest the readings coexist as parallel but non-contradictory methodological choices; locating it in innovation-blocking aggressiveness would suggest a sharper practical divergence in outcomes for customary and rationalist legal claims, strengthening the case for a more extractive reading of this constraint relative to its Maliki and Hanafi siblings.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Whether the kernel readings diverge mainly on source ranking, silence-recognition, or innovation-risk tolerance.').

omega_variable(
    sibling_reading_epsilon_comparability,
    'Should the epsilon values authored across the four sibling readings (Hanafi, Hanbali, Maliki, Shafi''i) be understood as directly comparable magnitudes on a shared extraction scale, or as reading-indexed values whose absolute levels reflect each reading''s own internal logic and are only meaningfully compared in relative ordering?',
    'Cross-reference all four constraint stories once authored and examine whether their epsilon values were derived using consistent criteria (same referent: the standing arrangement under contest, assessed by each reading''s own lights) or whether drift in authorial judgment across separate generation sessions introduced incomparability.',
    'If epsilon values are directly comparable, the Hanbali reading''s 0.52 can be read as ''more extractive than Maliki, less than a hypothetical maximally rigid reading'' with confidence; if not directly comparable, only the internal beneficiary/victim structure and relative ordering within this story should be relied upon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_epsilon_comparability, conceptual, 'Comparability of epsilon across independently-authored sibling kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(usul_al_fiqh_method__hanbali_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usul_tr_t0, usul_al_fiqh_method__hanbali_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(usul_tr_t0, projected).
narrative_ontology:measurement(usul_tr_t200, usul_al_fiqh_method__hanbali_reading, theater_ratio, 200, 0.13).
narrative_ontology:measurement_basis(usul_tr_t200, projected).
narrative_ontology:measurement(usul_tr_t400, usul_al_fiqh_method__hanbali_reading, theater_ratio, 400, 0.15).
narrative_ontology:measurement_basis(usul_tr_t400, projected).
narrative_ontology:measurement(usul_tr_t600, usul_al_fiqh_method__hanbali_reading, theater_ratio, 600, 0.18).
narrative_ontology:measurement_basis(usul_tr_t600, projected).
narrative_ontology:measurement(usul_tr_t900, usul_al_fiqh_method__hanbali_reading, theater_ratio, 900, 0.2).
narrative_ontology:measurement_basis(usul_tr_t900, projected).
narrative_ontology:measurement(usul_tr_t1200, usul_al_fiqh_method__hanbali_reading, theater_ratio, 1200, 0.22).
narrative_ontology:measurement_basis(usul_tr_t1200, projected).

% Extraction over time
narrative_ontology:measurement(usul_be_t0, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(usul_be_t0, projected).
narrative_ontology:measurement(usul_be_t200, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 200, 0.45).
narrative_ontology:measurement_basis(usul_be_t200, projected).
narrative_ontology:measurement(usul_be_t400, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 400, 0.48).
narrative_ontology:measurement_basis(usul_be_t400, projected).
narrative_ontology:measurement(usul_be_t600, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 600, 0.5).
narrative_ontology:measurement_basis(usul_be_t600, projected).
narrative_ontology:measurement(usul_be_t900, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 900, 0.51).
narrative_ontology:measurement_basis(usul_be_t900, projected).
narrative_ontology:measurement(usul_be_t1200, usul_al_fiqh_method__hanbali_reading, base_extractiveness, 1200, 0.52).
narrative_ontology:measurement_basis(usul_be_t1200, projected).

% Suppression requirement over time
narrative_ontology:measurement(usul_su_t0, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(usul_su_t0, projected).
narrative_ontology:measurement(usul_su_t200, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 200, 0.53).
narrative_ontology:measurement_basis(usul_su_t200, projected).
narrative_ontology:measurement(usul_su_t400, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 400, 0.55).
narrative_ontology:measurement_basis(usul_su_t400, projected).
narrative_ontology:measurement(usul_su_t600, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 600, 0.57).
narrative_ontology:measurement_basis(usul_su_t600, projected).
narrative_ontology:measurement(usul_su_t900, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 900, 0.59).
narrative_ontology:measurement_basis(usul_su_t900, projected).
narrative_ontology:measurement(usul_su_t1200, usul_al_fiqh_method__hanbali_reading, suppression_requirement, 1200, 0.61).
narrative_ontology:measurement_basis(usul_su_t1200, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(usul_al_fiqh_method__hanbali_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(usul_al_fiqh_method__hanbali_reading, 0.1).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, hanafi_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, maliki_reading).
narrative_ontology:affects_constraint(usul_al_fiqh_method__hanbali_reading, shafii_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four sibling readings of the usul_al_fiqh_method kernel, each authored as a separate story with its own epsilon and structural data per the epsilon-invariance principle. The Hanbali reading is linked bidirectionally to hanafi_reading, maliki_reading, and shafii_reading; contamination/purity propagation across these edges should be interpreted as tracking shared kernel legitimacy dynamics (e.g., broader disputes over the authority of usul al-fiqh as a meta-discipline), not as any one reading being derivative of the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
