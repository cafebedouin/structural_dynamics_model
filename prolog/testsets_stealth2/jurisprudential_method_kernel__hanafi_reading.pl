% ============================================================================
% CONSTRAINT STORY: jurisprudential_method_kernel__hanafi_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jurisprudential_method_kernel__hanafi_reading, []).

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
 *   constraint_id: jurisprudential_method_kernel__hanafi_reading
 *   human_readable: Hanafi Reading: Reason-Mediated Derivation of Divine Law (Qiyas and Istihsan)
 *   domain: legal/institutional/religious
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the jurisprudential-method kernel:
 *   the Hanafi arrangement in which law derives from Qur'an and Hadith only
 *   as filtered through extensive analogical reasoning (qiyas) and juristic
 *   preference (istihsan), with reason licensed as a legitimate instrument
 *   for extending divine intent to novel cases. The epsilon referent is the
 *   standing qiyas-and-istihsan-mediated arrangement itself — the structure
 *   of authority concentration, jurist-class reproduction, and textualist
 *   displacement that this reading actually builds — assessed on this
 *   reading's own structural facts; it is NOT the hypothetical pure-textual
 *   regime a rival reading would install, and epsilon is not hedged across
 *   readings. The arrangement carries a genuine coordination function
 *   (novel-case resolution and cross-empire legal consistency) AND asymmetric
 *   extraction (interpretive authority, office, and endowed income
 *   concentrate in the rationalist-trained jurist class, while the textualist
 *   claim to exclusive authenticity is structurally displaced and unmediated
 *   lay reading is stripped of legal standing). The claim/metric gap is
 *   deliberate: the school CLAIMS its method as faithful service to
 *   revelation, while the authored metrics describe substantially extractive,
 *   actively enforced operation — the engine measures that divergence. Family
 *   links run to the three sibling readings via network.affects_constraints;
 *   the doctrinal casualty named in the expected delta (the textualist
 *   exclusivity claim) is borne by the textualist scholar seat, since victims
 *   must be actors rather than propositions.
 *
 * KEY AGENTS:
 *   - rationalist_trained_jurists: Primary beneficiary and administrator (organized/identity_locked) — collects interpretive authority, runs the method, certifies successors
 *   - hanafi_madhhab_institution: Institutional beneficiary (institutional/identity_locked) — the school whose boundary IS the method
 *   - imperial_legal_administrations: Secondary beneficiary (institutional/mobile) — patron-states collecting administrability, retaining arbitrage-grade exit
 *   - textualist_hadith_scholars: Primary target (organized/constrained) — bears displacement of the authenticity claim
 *   - lay_direct_textualists: Target (powerless/trapped) — unmediated reading stripped of legal standing
 *   - governed_unseated_laity: Excluded seat (powerless/trapped) — governed by outputs, never seated on method legitimacy
 *   - comparative_usul_methodologists: Analytical observer (analytical/analytical) — sees the full structure across schools
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jurisprudential_method_kernel__hanafi_reading, 0.66).
domain_priors:suppression_score(jurisprudential_method_kernel__hanafi_reading, 0.62).
domain_priors:theater_ratio(jurisprudential_method_kernel__hanafi_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(jurisprudential_method_kernel__hanafi_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jurisprudential_method_kernel__hanafi_reading, tangled_rope).
narrative_ontology:human_readable(jurisprudential_method_kernel__hanafi_reading, "Hanafi Reading: Reason-Mediated Derivation of Divine Law (Qiyas and Istihsan)").
narrative_ontology:topic_domain(jurisprudential_method_kernel__hanafi_reading, "legal/institutional/religious").

domain_priors:requires_active_enforcement(jurisprudential_method_kernel__hanafi_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jurisprudential_method_kernel__hanafi_reading, '29d1eb45-db3d-4eb1-a196-716e144f8728').
narrative_ontology:cs_kernel_codification('29d1eb45-db3d-4eb1-a196-716e144f8728', formalized).
narrative_ontology:cs_authority_grounding('29d1eb45-db3d-4eb1-a196-716e144f8728', lineage).
narrative_ontology:cs_interpretation_layer_present('29d1eb45-db3d-4eb1-a196-716e144f8728').
narrative_ontology:cs_reading_relation('29d1eb45-db3d-4eb1-a196-716e144f8728', jurisprudential_method_kernel__maliki_reading, coexists_with).
narrative_ontology:cs_reading_relation('29d1eb45-db3d-4eb1-a196-716e144f8728', jurisprudential_method_kernel__shafii_reading, influences).
narrative_ontology:cs_reading_relation('29d1eb45-db3d-4eb1-a196-716e144f8728', jurisprudential_method_kernel__hanbali_reading, forecloses).
narrative_ontology:cs_axiom('29d1eb45-db3d-4eb1-a196-716e144f8728', foundational, analogical_extension_preserves_divine_intent).
narrative_ontology:cs_axiom_status(analogical_extension_preserves_divine_intent, holdable).
narrative_ontology:cs_axiom_grounding('29d1eb45-db3d-4eb1-a196-716e144f8728', analogical_extension_preserves_divine_intent, theological).
narrative_ontology:cs_axiom('29d1eb45-db3d-4eb1-a196-716e144f8728', foundational, juristic_equity_is_valid_derivation).
narrative_ontology:cs_axiom_status(juristic_equity_is_valid_derivation, holdable).
narrative_ontology:cs_axiom_grounding('29d1eb45-db3d-4eb1-a196-716e144f8728', juristic_equity_is_valid_derivation, instrumental).
narrative_ontology:cs_reference_frame('29d1eb45-db3d-4eb1-a196-716e144f8728', divine_intent_rationally_extendable).
narrative_ontology:cs_drift_state('29d1eb45-db3d-4eb1-a196-716e144f8728', contemporary_nation_state_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('29d1eb45-db3d-4eb1-a196-716e144f8728', '').
narrative_ontology:cs_kernel_id(jurisprudential_method_kernel__hanafi_reading, jurisprudential_method_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, rationalist_trained_jurists).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, hanafi_madhhab_institution).
narrative_ontology:constraint_beneficiary(jurisprudential_method_kernel__hanafi_reading, imperial_legal_administrations).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, textualist_hadith_scholars).
narrative_ontology:constraint_victim(jurisprudential_method_kernel__hanafi_reading, lay_direct_textualists).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanafi_reading, validity_of_qiyas).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanafi_reading, validity_of_istihsan).
narrative_ontology:constraint_vindicates(jurisprudential_method_kernel__hanafi_reading, divine_intent_rationally_extendable).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars formed through madrasa chains in usul al-fiqh who derive rulings by analogical extension and equity-adjusted preference. They staff judgeships and endowed teaching posts, certify successors, write the manuals that define valid method, and decide which novel questions reach derivation at all. Their entire epistemic capital is the method itself: leaving it means abandoning professional identity, standing, and livelihood at once.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, rationalist_trained_jurists, beneficiary,
    organized, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(jurisprudential_method_kernel__hanafi_reading, rationalist_trained_jurists, agenda_setter).

% The trans-generational school descending from Abu Hanifa through Abu Yusuf and al-Shaybani. Its boundary and prestige are constituted by methodological distinctiveness: what separates it from rival schools is precisely the breadth of its license to reason. It collects endowments, appointments, and deference across Central Asia, Anatolia, the Balkans, Egypt, and South Asia, and cannot adopt a rival method without ceasing to be itself.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, hanafi_madhhab_institution, beneficiary,
    institutional, civilizational, identity_locked, continental).

% Abbasid and later Ottoman states that patronize the school because its reasoned flexibility scales: trained judges in distant provinces reach consistent, administrable outcomes on commerce, taxation, and procedure, and the center steers the judiciary through appointment. Patronage is a choice rather than a bond; these administrations shifted support among schools historically and retain arbitrage-grade exit.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, imperial_legal_administrations, beneficiary,
    institutional, generational, mobile, continental).

% Scholars who ground legal authority in the literal text of revelation and companion report, holding that analogical extension and juristic preference are corrupting innovations. Wherever the reasoned method governs the courts, their claim to exclusive authenticity loses standing and their students face a choice: convert to the method and dissolve their distinct position, or remain outside and absorb institutional exclusion. Over generations many accommodated a narrowed, licensed analogy under sustained pressure.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, textualist_hadith_scholars, payer,
    organized, generational, constrained, continental).

% Believers who read scripture directly and reach their own conclusions. Under this arrangement those conclusions carry no legal weight unless ratified by trained derivation; private reasoning has no standing in any court or council. There is no exit from needing law, and no channel through which unmediated reading acquires force.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, lay_direct_textualists, payer,
    powerless, biographical, trapped, continental).

% The mass of believers who live under the derived rules but never sat in the methodological councils where the legitimacy of analogical extension and juristic preference was argued and settled. Their consent was never solicited; they enter the record only as subjects of rulings, family-law outcomes, and commercial determinations made by others about the method itself.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, governed_unseated_laity, excluded,
    powerless, biographical, trapped, continental).

% Later methodologists in the lineage of al-Ghazali and al-Amidi who compared the derivation logics of all schools analytically, cataloguing where each admits reason, practice, or transmission. Holding no stake in any single school's offices or endowments, they can see the whole structure of competing methods at once.
narrative_ontology:constraint_stakeholder(jurisprudential_method_kernel__hanafi_reading, comparative_usul_methodologists, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jurisprudential_method_kernel__hanafi_reading, rationalist_trained_jurists).
narrative_ontology:fixing_cost_class(jurisprudential_method_kernel__hanafi_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of applying a finite revealed corpus to indefinitely novel cases — new contracts, procedures, technologies, imperial commerce — while preserving continuity with revelation, and gives geographically dispersed judges a shared derivable method so that distant courts reach consistent outcomes.
% TRANSFER_FUNCTION: Moves interpretive authority, and the income, office, and status attached to it, from untrained readers and textual rivals to credentialed jurists; every novel legal question is moved out of the community at large and into the jurist class's monopoly of derivation.
% ABSENT_VOICES: The governed laity were never seated when the method's legitimacy was settled — the argument was conducted among jurists, for jurists, over the heads of those who would live under its outputs. Women litigants whose family-law outcomes turn on juristic-preference calls had no procedural voice. Textualist objectors were present in the debate but were out-institutionalized rather than persuaded; their dissent survives in writing, not in seating.
% DISAPPEARANCE_RATIONALE: If the reasoned-derivation arrangement vanished overnight, every case not covered by explicit text loses its resolution path: courts stall or fall back on raw textualism and unlicensed personal opinion, the jurist class's function and income evaporate, the school's boundary dissolves, and the competitive landscape among the sibling methods collapses — the entire legal economy of the Hanafi world reorganizes around whichever rival channel inherits the novelty problem.
% FOUNDING_PROBLEM: Novel cases beyond the revealed text demanded resolution without either abandoning revelation or licensing arbitrary opinion; Abu Hanifa's Kufan circle faced an expanding empire-scale commercial and administrative docket that the bare text could not reach, and built a disciplined way for reason to extend divine intent.
% FOUNDING_PROBLEM_CORROBORATION: Textualist scholars themselves concede the novelty problem is real even while disputing this solution — they answer it with companion precedent and consensus instead, which corroborates the problem from outside the benefiting parties. Comparative historians of Islamic law document novelty-resolution as the documented driver of usul al-fiqh's development, and the fact that all four sibling schools built different machinery for the same problem attests that the underlying problem persists independently of any one school's answer.
narrative_ontology:disappearance_verdict(jurisprudential_method_kernel__hanafi_reading, world_rearranges).
narrative_ontology:founding_problem_status(jurisprudential_method_kernel__hanafi_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jurisprudential_method_kernel__hanafi_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jurisprudential_method_kernel__hanafi_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jurisprudential_method_kernel__hanafi_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jurisprudential_method_kernel__hanafi_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jurisprudential_method_kernel__hanafi_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jurisprudential_method_kernel__hanafi_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.66) because the method converts every novel case into a demand for credentialed mediation: the scarcity is manufactured by the method's own complexity threshold, and the rents (judgeships, teaching chairs, waqf income) accrue to the class that administers the threshold. Suppression (0.62) is institutional rather than violent — rival channels are marginalized through certification gates, qadi appointment exclusivity, and curricular control, not eliminated, which is why accessibility_collapse stays moderate (0.45): the sibling schools remain live alternatives, and that survival is exactly what keeps this a hybrid rather than a pure-extraction structure. Theater (0.28) reflects late-period taqlid ritualization — a growing share of activity repeats inherited doctrine rather than exercising fresh derivation — but the core function stayed live across the interval, so theater remains a minority fraction. Resistance (0.55) records a millennium of sustained textualist polemic against qiyas and especially istihsan. The temporal series run on ONE shared grid (all three metrics at t = 0, 200, 400, 600, 800, 1000, 1200, roughly 750–1950 CE) so no metric borrows another's endpoint. Base extractiveness accumulates as the class entrenches; suppression_requirement rises because this story specifically tracks enforcement-capacity hardening (madrasa certification from the eleventh century, madhhab-exclusive judiciaries, Ottoman integration of fiqh with kanun) — a maturing enforcement ratchet, not merely shifting extraction; theater climbs with taqlid-era formalization.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the jurist seat the arrangement is a sacred craft and a public service — the method is their identity, and its demands feel like devotion rather than rent; with identity_locked exit, their maintenance stakes are amplified regardless of net flow. From the textualist seat the same structure operates as the delegitimation machine that strips their authenticity claim of standing. From the trapped lay seat it is a closed gate: reading the text directly yields nothing enforceable. The patron-state seat experiences flexibility and administrability and can leave. The engine computes these divergent classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Rationalist-trained jurists sit nearest the beneficiary end: they collect the transfer and run the machinery, and identity-lock deepens their stake in persistence. The madhhab institution likewise. Imperial administrations derive low directionality through genuine benefit but their mobility keeps them from fusing with the arrangement. Textualist scholars derive high directionality as declared victims with constrained exit — conversion dissolves their position, persistence costs them standing. Lay direct readers derive the highest directionality: trapped, powerless, bearing the denial of unmediated access with no alternative forum. The excluded laity seat feeds the consensus-provenance check rather than directionality. No directionality overrides are needed: the beneficiary/victim declarations plus exit atoms already produce the correct spread, and the dual-positioned jurist seat (beneficiary administering its own benefit) is encoded through secondary_role rather than override.
 *
 * MANDATROPHY ANALYSIS:
 *   There is no mandatrophy here: the founding problem — resolving cases the text does not reach — is permanently live, because novelty never stops; the arrangement's justification remains its steady-state operation, not a completed transition. The classification discipline cuts both ways. Calling this a snare would erase the real coordination function (novel-case resolution and empire-scale consistency) that even hostile sibling schools implicitly concede by building their own machinery for the same problem. Calling it a rope would erase the authority rent and the displaced textualist claim. Tangled rope holds both truths: genuine coordination AND asymmetric extraction through the same structure, held in place by active enforcement. The taqlid-era theater rise is flagged as partial mandate-atrophy risk — the derivation function narrowing into repetition — without asserting full atrophy, since fresh fatwa reasoning persisted throughout.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'Is the measured extraction a property of reason-mediated derivation as such, or of THIS reading''s particular institutional history (entrenchment, taqlid, patronage)? This constraint is one reading of jurisprudential_method_kernel; a sibling reading would change the beneficiary set (Medinan practitioners, hadith-transmission specialists, consensus-holders), the extraction surface, and possibly the epsilon profile entirely.',
    'Generate the three sibling stories and compare epsilon trajectories across the kernel: if all four readings show similar accumulation curves, extraction belongs to juristic mediation as such; if the Hanafi profile is uniquely steep, it belongs to this reading''s specific structure of discretionary breadth.',
    'Cross-kernel comparison determines whether reform should target the method-family or this reading''s institutional form; misattributing the source would misdirect any remedy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed versus kernel-level attribution of the measured extraction.').

omega_variable(
    istihsan_qiyas_extraction_split,
    'Do disciplined analogy (qiyas) and open juristic preference (istihsan) carry different extraction within this single reading? Istihsan grants wider unstructured discretion to the jurist, which plausibly concentrates more authority rent than rule-bound analogy; if measuring them separately yields materially different epsilon values, this is two constraints sharing one label.',
    'Separate analysis of rulings sourced primarily through istihsan versus pure qiyas, tracing which channel correlates with concentrated discretion and contested outcomes; if the epsilon values diverge, decompose into two linked stories per the epsilon-invariance principle.',
    'Decomposition would isolate equity-discretion as the extractive core and leave disciplined analogy closer to coordination cost, changing both the classification target and any remedy design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(istihsan_qiyas_extraction_split, conceptual, 'Internal epsilon-invariance check: whether the reading''s two reasoning instruments are one constraint or two.').

omega_variable(
    taqlid_gate_closing_reality,
    'Was the late-period hardening (rising theater and enforcement intensity after t=600) a real structural closure of independent derivation, or a historiographical artifact of the ''closed gate of ijtihad'' thesis?',
    'Prosopographic study of post-classical jurists claiming ijtihad, and manuscript analysis of fresh fatwa reasoning versus reproductive commentary across the tenth through nineteenth centuries.',
    'If the hardening is artifact, the theater and suppression trajectories overstate drift and the arrangement remained functionally open longer than modeled; if real, the piton-drift risk is greater than the terminal values suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taqlid_gate_closing_reality, empirical, 'Whether late-period ritualization reflects genuine functional atrophy or scholarly caricature.').

omega_variable(
    post_interval_codification_trajectory,
    'Beyond the interval''s end, do modern statutory codes absorb the jurist-rent structure (state-employed muftis reproducing it under new auspices) or abolish it (derivation authority passing to legislatures)?',
    'Comparative study of twentieth-century legal reforms in formerly Hanafi jurisdictions — the Ottoman Mecelle, Turkish replacement, South Asian Anglo-Muhammadan law — tracking where derivation authority actually resettled.',
    'Absorption predicts persistence of the extraction structure under new administration; abolition predicts decay toward inertial performance, dating a potential tangled_rope-to-piton transition after the modeled interval.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(post_interval_codification_trajectory, empirical, 'Open trajectory of the arrangement under modern codification, outside the authored interval.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jurisprudential_method_kernel__hanafi_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(juri_tr_t0, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(juri_tr_t0, observed).
narrative_ontology:measurement(juri_tr_t200, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 200, 0.1).
narrative_ontology:measurement_basis(juri_tr_t200, observed).
narrative_ontology:measurement(juri_tr_t400, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 400, 0.13).
narrative_ontology:measurement_basis(juri_tr_t400, observed).
narrative_ontology:measurement(juri_tr_t600, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 600, 0.17).
narrative_ontology:measurement_basis(juri_tr_t600, observed).
narrative_ontology:measurement(juri_tr_t800, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 800, 0.21).
narrative_ontology:measurement_basis(juri_tr_t800, observed).
narrative_ontology:measurement(juri_tr_t1000, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 1000, 0.25).
narrative_ontology:measurement_basis(juri_tr_t1000, observed).
narrative_ontology:measurement(juri_tr_t1200, jurisprudential_method_kernel__hanafi_reading, theater_ratio, 1200, 0.28).
narrative_ontology:measurement_basis(juri_tr_t1200, observed).

% Extraction over time
narrative_ontology:measurement(juri_be_t0, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 0, 0.44).
narrative_ontology:measurement_basis(juri_be_t0, observed).
narrative_ontology:measurement(juri_be_t200, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 200, 0.5).
narrative_ontology:measurement_basis(juri_be_t200, observed).
narrative_ontology:measurement(juri_be_t400, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 400, 0.55).
narrative_ontology:measurement_basis(juri_be_t400, observed).
narrative_ontology:measurement(juri_be_t600, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 600, 0.59).
narrative_ontology:measurement_basis(juri_be_t600, observed).
narrative_ontology:measurement(juri_be_t800, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 800, 0.62).
narrative_ontology:measurement_basis(juri_be_t800, observed).
narrative_ontology:measurement(juri_be_t1000, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 1000, 0.64).
narrative_ontology:measurement_basis(juri_be_t1000, observed).
narrative_ontology:measurement(juri_be_t1200, jurisprudential_method_kernel__hanafi_reading, base_extractiveness, 1200, 0.66).
narrative_ontology:measurement_basis(juri_be_t1200, observed).

% Suppression requirement over time
narrative_ontology:measurement(juri_su_t0, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(juri_su_t0, observed).
narrative_ontology:measurement(juri_su_t200, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 200, 0.44).
narrative_ontology:measurement_basis(juri_su_t200, observed).
narrative_ontology:measurement(juri_su_t400, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 400, 0.49).
narrative_ontology:measurement_basis(juri_su_t400, observed).
narrative_ontology:measurement(juri_su_t600, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 600, 0.53).
narrative_ontology:measurement_basis(juri_su_t600, observed).
narrative_ontology:measurement(juri_su_t800, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 800, 0.57).
narrative_ontology:measurement_basis(juri_su_t800, observed).
narrative_ontology:measurement(juri_su_t1000, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 1000, 0.6).
narrative_ontology:measurement_basis(juri_su_t1000, observed).
narrative_ontology:measurement(juri_su_t1200, jurisprudential_method_kernel__hanafi_reading, suppression_requirement, 1200, 0.62).
narrative_ontology:measurement_basis(juri_su_t1200, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jurisprudential_method_kernel__hanafi_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, maliki_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, shafii_reading).
narrative_ontology:affects_constraint(jurisprudential_method_kernel__hanafi_reading, hanbali_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Islamic jurisprudential method' conflates four structurally distinct arrangements that instantiate one contested kernel. This file is the Hanafi member (broad reason-license via qiyas and istihsan); the Maliki member routes extension through Medinan living practice, the Shafi'i member through a strict transmission-arbitrated hierarchy, and the Hanbali member denies reason-extension outright. Epsilon differs across members because the beneficiary sets and discretion structures differ; the upstream/downstream edges record mutual structural pressure (Hanafi practice provoked Shafi'i's standardization; Hanbali rejection disciplines everyone else's discretion claims) without merging the stories. Each member keeps its own epsilon, stakeholders, and classification per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
