% ============================================================================
% CONSTRAINT STORY: us_constitution_text__originalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_text__originalist_reading, []).

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
 *   constraint_id: us_constitution_text__originalist_reading
 *   human_readable: Originalist Reading: Constitutional Meaning Fixed at Ratification
 *   domain: legal/constitutional/interpretive_theory
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel: the
 *   originalist_reading of the United States Constitution's text, under which
 *   constitutional meaning was fixed at ratification and judicial
 *   interpretation must recover the original public understanding rather than
 *   adapt it. The standing arrangement under contest — and the sole epsilon
 *   referent for this file — is that fixed-meaning interpretive discipline as
 *   it now operates on federal adjudication. The living-constitutionalist and
 *   positivist readings are separate constraint stories with their own
 *   epsilon values, beneficiary structures, and victim sets, linked through
 *   the network section; per the epsilon-invariance principle this file does
 *   not hedge across readings or average over them. The discipline has a
 *   genuine coordination function (it gives judges a determinate,
 *   evidence-based standard external to their values and channels contested
 *   value questions to elected institutions) and a genuine asymmetric cost
 *   structure (it closes federal constitutional claims lacking founding-era
 *   analogues and consolidates interpretive gatekeeping in the movement that
 *   supplies the method's judges and history). The claim/metrics split is
 *   deliberate: claimed_type is authored from structural analysis; the
 *   metrics are authored independently as descriptive measurements of the
 *   discipline's actual operation, and the engine computes each seat's type
 *   from the structural data. KEY AGENTS (by structural relationship): -
 *   originalist_court_majority: agenda-setter/enforcer
 *   (institutional/identity_locked) — writes and polices the historical
 *   standard - conservative_legal_movement: primary beneficiary and
 *   enforcement pipeline (institutional/arbitrage) — supplies judges,
 *   doctrine, and historical evidence - non_historical_rights_claimants:
 *   primary target (powerless/trapped) — federal claims close where
 *   founding-era analogues are absent - living_constitutionalist_jurists:
 *   secondary target, dissenting seat (powerful/identity_locked) — bound by a
 *   method they reject - elected_branches: secondary beneficiary
 *   (powerful/mobile) — gains policy space under a fixed baseline -
 *   founding_era_practice_litigants: secondary beneficiary (organized/mobile)
 *   — the historical record is their case - professional_historians:
 *   analytical observer (analytical/analytical) — documents divergence
 *   between judicial and disciplinary history - future_rights_claimants:
 *   excluded seat (powerless/trapped) — measured against a baseline fixed
 *   without them
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_text__originalist_reading, 0.65).
domain_priors:suppression_score(us_constitution_text__originalist_reading, 0.75).
domain_priors:theater_ratio(us_constitution_text__originalist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(us_constitution_text__originalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_text__originalist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_text__originalist_reading, "Originalist Reading: Constitutional Meaning Fixed at Ratification").
narrative_ontology:topic_domain(us_constitution_text__originalist_reading, "legal/constitutional/interpretive_theory").

domain_priors:requires_active_enforcement(us_constitution_text__originalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_text__originalist_reading, 'eafb74b8-b1fd-48f1-a0e4-7acbb6345a14').
narrative_ontology:cs_kernel_codification('eafb74b8-b1fd-48f1-a0e4-7acbb6345a14', fixed_text).
narrative_ontology:cs_authority_grounding('eafb74b8-b1fd-48f1-a0e4-7acbb6345a14', lineage).
narrative_ontology:cs_interpretation_layer_present('eafb74b8-b1fd-48f1-a0e4-7acbb6345a14').
narrative_ontology:cs_reading_relation('eafb74b8-b1fd-48f1-a0e4-7acbb6345a14', us_constitution_text__living_constitutionalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('eafb74b8-b1fd-48f1-a0e4-7acbb6345a14', us_constitution_text__positivist_reading, coexists_with).
narrative_ontology:cs_axiom('eafb74b8-b1fd-48f1-a0e4-7acbb6345a14', foundational, original_public_meaning_constitutes_law).
narrative_ontology:cs_axiom_status(original_public_meaning_constitutes_law, holdable).
narrative_ontology:cs_axiom_grounding('eafb74b8-b1fd-48f1-a0e4-7acbb6345a14', original_public_meaning_constitutes_law, conventional).
narrative_ontology:cs_axiom('eafb74b8-b1fd-48f1-a0e4-7acbb6345a14', secondary, historical_inquiry_supersedes_moral_adaptation).
narrative_ontology:cs_axiom_status(historical_inquiry_supersedes_moral_adaptation, holdable).
narrative_ontology:cs_axiom_grounding('eafb74b8-b1fd-48f1-a0e4-7acbb6345a14', historical_inquiry_supersedes_moral_adaptation, instrumental).
narrative_ontology:cs_reference_frame('eafb74b8-b1fd-48f1-a0e4-7acbb6345a14', ratification_era_public_understanding).
narrative_ontology:cs_drift_state('eafb74b8-b1fd-48f1-a0e4-7acbb6345a14', contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('eafb74b8-b1fd-48f1-a0e4-7acbb6345a14', '').
narrative_ontology:cs_kernel_id(us_constitution_text__originalist_reading, us_constitution_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, conservative_legal_movement).
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, elected_branches).
narrative_ontology:constraint_beneficiary(us_constitution_text__originalist_reading, founding_era_practice_litigants).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, non_historical_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_text__originalist_reading, living_constitutionalist_jurists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sits on the Supreme Court and writes the opinions that apply the fixed-at-ratification standard: announcing the historical test, policing lower-court adherence, and deciding which founding-era evidence counts. Each Justice arrived through a nomination process that screened for commitment to the method. Leaving would mean publicly renouncing the opinions they wrote and the confirmation coalition that seated them; their judicial identity and the method are now the same thing.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, originalist_court_majority, agenda_setter,
    institutional, generational, identity_locked, national).

% A network of lawyers, academics, advocacy organizations, and judicial pipeline institutions built the appointment screening, the law-school infrastructure, and the amicus apparatus that supply both the judges and the historical evidence the standard runs on. It collects institutional dominance: its method names the operative test, its members hold the seats, and its organizations shape which history reaches the Court. In principle it could pivot to a different interpretive method if the returns shifted; in practice its identity, funding, and multi-decade investment are bound to this one.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, conservative_legal_movement, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_text__originalist_reading, conservative_legal_movement, agenda_setter).

% Litigants whose asserted rights have no close founding-era analogue — claims to bodily autonomy, modern equality interests, new technologies of expression or surveillance — must win by persuading a court that the public understanding of 1791 or 1868 protects them. Where the historical record is silent or adverse, their federal constitutional claim closes regardless of its present-day weight. Their alternatives are legislative lobbying or state constitutional litigation, neither of which reaches the federal right they asserted.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, non_historical_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Judges and justices trained in and committed to adaptive interpretation now write dissents, distinguish historical tests where precedent allows, and preserve adaptive doctrine at the margins. Their professional identity is constituted through the method they defend; converting would cost them their scholarly standing, their coalition, and their self-conception. They bear the standard's operation in every opinion they must join, apply, or answer.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, living_constitutionalist_jurists, payer,
    powerful, biographical, identity_locked, national).

% Congress, presidents, and state legislatures gain policy space when constitutional review ties to a fixed historical baseline rather than evolving judicial values: more statutes survive review, and contested value questions route back to the political process. The flow is contingent — a historical test also invalidates modern laws, as in firearms regulation — but on net the arrangement returns discretion to elected institutions.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, elected_branches, beneficiary,
    powerful, generational, mobile, national).

% Litigants whose claims track founding-era arrangements — criminal defendants invoking founding protections, organizations litigating arms-bearing rights through founding-era analogues — gain a structured path to victory: the historical record is their case. Well-resourced repeat litigants commission the history that wins, and the method rewards those who can fund it.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, founding_era_practice_litigants, beneficiary,
    organized, biographical, mobile, national).

% Academic historians of the founding era file amicus briefs, publish critiques of advocacy-commissioned history, and document where judicial historical method diverges from disciplinary standards. They hold no vote and their findings bind no court; their seat is analytical, watching the evidence base the standard depends on be assembled and contested.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, professional_historians, observer,
    analytical, civilizational, analytical, national).

% People not yet before a court, including generations not yet born, will have their asserted rights measured against a public understanding recorded before they existed. They were present at no ratification, commissioned no history, and hold no seat in the interpretive conversation; the baseline was fixed without them and applies to them regardless.
narrative_ontology:constraint_stakeholder(us_constitution_text__originalist_reading, future_rights_claimants, excluded,
    powerless, civilizational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_text__originalist_reading, conservative_legal_movement).
narrative_ontology:fixing_cost_class(us_constitution_text__originalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Constrains federal judges to a determinate, evidence-based standard: the written Constitution's ratification-era public meaning. It addresses the problem of unbounded judicial discretion by giving litigants a rule external to judges' values, and it routes contested value questions to elected institutions.
% TRANSFER_FUNCTION: Moves interpretive authority and win-probability in constitutional litigation: from claimants whose rights lack founding-era analogues toward parties whose positions track founding-era arrangements, while consolidating the supply of controlling historical evidence and of judges within the movement that built the method.
% ABSENT_VOICES: Future rights claimants and the ratification-era public itself — the 'original understanding' is reconstructed by advocates and judges, never represented by it. Professional historians complicate the record but hold no binding seat. Non-historical claimants appear only as litigants seeking exceptions to a rule written against them.
% DISAPPEARANCE_RATIONALE: If the fixed-meaning discipline vanished overnight, doctrines built on historical tests would lose their warrant and need re-derivation, the appointment pipeline's organizing criterion would collapse, and litigation strategy, law-school curricula, and judicial coalitions would reorganize around whichever interpretive method next held the Court. Thousands of settled outcomes rest on the arrangement's continued operation.
% FOUNDING_PROBLEM: The countermajoritarian difficulty: how unelected judges can invalidate the acts of elected majorities without becoming an unelected legislature. The modern discipline consolidated as a specific answer to the adaptive rights rulings of the mid-twentieth-century Court.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: living-constitutionalist theorists (the Ely line) and progressive jurists accept the countermajoritarian difficulty as real and propose rival answers to it; the problem is the shared starting point of modern constitutional theory, not a claim only the method's beneficiaries make.
narrative_ontology:disappearance_verdict(us_constitution_text__originalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_text__originalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_text__originalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_text__originalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_text__originalist_reading, 0.65, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_text__originalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_constitution_text__originalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(us_constitution_text__originalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.65: the discipline does real coordinative work, but its cost structure is asymmetric — win-probability in rights litigation transfers systematically toward historically grounded positions, and the transfer has grown as the method captured seats and doctrine (Heller 2008 made the historical test operative; Bruen 2022 extended it; Dobbs 2022 removed an adaptive right). Suppression 0.75: persistence depends on actively excluding adaptive method — through appointment screening, confirmation politics, and doctrinal policing — not on participant consensus; alternatives survive in the academy, in dissents, and in state courts but are barred from operative federal force. Theater_ratio 0.25: historical inquiry is genuine labor, but a rising share is instrumental (advocacy-commissioned 'law office history,' amicus history wars), which the measurement series tracks upward from 0.10 to 0.25. Accessibility_collapse 0.50: within federal constitutional adjudication, alternatives collapse substantially once the method is entrenched; outside it (state constitutions, legislatures, scholarship) they persist, so collapse is partial. Resistance 0.60: sustained scholarly critique, dissents, and litigation meet the standard continuously; it holds through structural entrenchment rather than agreement. The suppression_requirement series is authored deliberately: this story's arc IS enforcement-capacity buildup (movement founding, pipeline maturation, seat capture, doctrinal consolidation), so a rising enforcement trajectory from 0.35 to 0.75 is the dynamic under trace. All three series share one time grid (1982-2025, seven points) so the engine samples every metric at every point without scalar substitution.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter/beneficiary seats should compute differently. From the court-majority and movement seats the arrangement is a legitimacy structure they built and are bound by voluntarily — fidelity to enacted law, constraint on discretion. From the non-historical-claimant seat the same structure operates as a closed door: the claim fails on the record's silence, not its merits. The sharpest divergence is same-level lateral: living_constitutionalist_jurists and the originalist majority hold nominally identical institutional power (federal judgeships, life tenure), but the majority seat wields the standard while the dissent seat bears it — differentiated by identity_lock (each seat's professional self-conception is fused to its method, making conversion costlier than dissent) and by role in the appointment pipeline. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: non_historical_rights_claimants (declared victim, trapped exit) sit near the full-target end; living_constitutionalist_jurists (declared victim, identity_locked) sit near full target; founding_era_practice_litigants and elected_branches (declared beneficiaries, mobile exit) sit near the beneficiary end. One override is authored: power_atom institutional, d 0.20. Rationale: the canonical fallback for institutional-power agents would place them near-symmetric, but both institutional seats in this story — originalist_court_majority and conservative_legal_movement — are enforcer-beneficiaries whose returns (legitimacy, dominance, seat control) accrue from the arrangement's operation. The movement's derived d from its beneficiary declaration would be near the beneficiary end already; the override holds it slightly above pure-beneficiary because its arbitrage-grade exit is nominal — its multi-decade identity and infrastructure investment are sunk into this method specifically. The court majority's fallback would misplace it near-symmetric; the override corrects it toward the beneficiary end, reflecting that the discipline binds its wielders only in the weak sense that it is their own commitment. No override is needed for the powerless/powerful seats: victim-plus-trapped and victim-plus-identity_locked derivations already land them at the target end.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the countermajoritarian difficulty — is live and corroborated across the interpretive spectrum, so no mandatrophy is declared and the mismatch consumer should read status=live x verdict=world_rearranges as coherent (no zombie flag). The tangled_rope classification does the preventive work in both directions: it blocks the movement's own framing from reading as pure coordination (the transfer is asymmetric and enforced, with named payers), and it blocks a purely extractive reading from erasing the genuine coordination function (the discipline sincerely constrains judges, including against outcomes its beneficiaries sometimes dislike, and the rule-of-law problem it addresses is real). The low theater_ratio and the absence of any sunset clause confirm the arrangement is maintained, not inertial: this is not a degraded former coordination being performed, but an actively enforced hybrid whose coordination and extraction components are separable only empirically (see the historical_evidence_gatekeeping omega).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates the originalist_reading of the us_constitution_text kernel; how would the structural data and computed classification change under each sibling reading?',
    'Generate the sibling stories (us_constitution_text__living_constitutionalist_reading, us_constitution_text__positivist_reading) from the same kernel and compare beneficiary/victim sets, epsilon, and computed types across the family.',
    'The living reading would invert the beneficiary/victim structure — adaptive-rights claimants become beneficiaries and historical-practice defenders become the bound party — and would author a different epsilon over the same ratification-fixation arrangement. Classification divergence across the family is the measurement the corpus exists to take, not an error to reconcile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one reading of a three-reading kernel; the contest lives between files, not inside this one.').

omega_variable(
    historical_evidence_gatekeeping,
    'Is the discipline''s extractive share driven by the fixed-meaning method itself, or by the movement''s gatekeeping over which historical evidence and which historians reach the Court?',
    'Compare case outcomes and reasoning where disciplinary-historian consensus dominates the briefing against cases where advocacy-commissioned history dominates; natural experiment across jurisdictions with differing historical-record practices.',
    'If the method is evidence-neutral, epsilon falls toward the coordination floor and the tangled_rope reading softens toward rope; if extraction is supply-side capture of the evidence base, the snare share rises and the movement''s agenda-setter role becomes the operative extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_evidence_gatekeeping, empirical, 'Whether the measured extraction tracks the method or control of its evidence supply.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression of adaptive interpretation structural (appointment screening, doctrinal policing, confirmation politics) or internalized (jurists and scholars self-police because the method has fused with professional identity)?',
    'Post-exit suppression trajectory: observe state courts, international tribunals, and the academy, where the federal appointment machinery does not reach — if adaptive method persists there among similarly trained jurists, the suppression is structural; if jurists carry the self-policing across institutional boundaries, it is internalized.',
    'If substantially internalized, the discipline''s effective suppression exceeds its structural enforcement measure and would persist through personnel turnover; if structural, personnel change could relax it quickly. The omega feeds the structural-vs-internalized ambiguity the scalar suppression metric cannot distinguish.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized mechanism for the suppression of adaptive interpretation.').

omega_variable(
    dead_hand_binding_legitimacy,
    'Can ratification-era public understanding legitimately bind present and future generations who never consented to it — and does the answer change the discipline''s coordination warrant?',
    'Not resolvable by data alone: it turns on contested commitments about popular sovereignty, intergenerational obligation, and whether super-majoritarian enactment at one moment can ground perpetual content-fixation. Resolution arrives as a preference settlement (amendment practice, constitutional replacement, or doctrinal drift), not a measurement.',
    'If the dead-hand objection prevails, the discipline''s coordination warrant collapses and its persistence reads as enforced extraction without coordination cover, pushing the classification toward snare; if the objection is rejected, the warrant holds and the tangled_rope reading stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dead_hand_binding_legitimacy, preference, 'Whether intergenerational binding is a legitimate coordination function or a consent failure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_text__originalist_reading, 1982, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1982, us_constitution_text__originalist_reading, theater_ratio, 1982, 0.1).
narrative_ontology:measurement(us_c_tr_t1991, us_constitution_text__originalist_reading, theater_ratio, 1991, 0.13).
narrative_ontology:measurement(us_c_tr_t2000, us_constitution_text__originalist_reading, theater_ratio, 2000, 0.16).
narrative_ontology:measurement(us_c_tr_t2008, us_constitution_text__originalist_reading, theater_ratio, 2008, 0.18).
narrative_ontology:measurement(us_c_tr_t2016, us_constitution_text__originalist_reading, theater_ratio, 2016, 0.2).
narrative_ontology:measurement(us_c_tr_t2022, us_constitution_text__originalist_reading, theater_ratio, 2022, 0.23).
narrative_ontology:measurement(us_c_tr_t2025, us_constitution_text__originalist_reading, theater_ratio, 2025, 0.25).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1982, us_constitution_text__originalist_reading, base_extractiveness, 1982, 0.28).
narrative_ontology:measurement(us_c_be_t1991, us_constitution_text__originalist_reading, base_extractiveness, 1991, 0.34).
narrative_ontology:measurement(us_c_be_t2000, us_constitution_text__originalist_reading, base_extractiveness, 2000, 0.4).
narrative_ontology:measurement(us_c_be_t2008, us_constitution_text__originalist_reading, base_extractiveness, 2008, 0.48).
narrative_ontology:measurement(us_c_be_t2016, us_constitution_text__originalist_reading, base_extractiveness, 2016, 0.55).
narrative_ontology:measurement(us_c_be_t2022, us_constitution_text__originalist_reading, base_extractiveness, 2022, 0.63).
narrative_ontology:measurement(us_c_be_t2025, us_constitution_text__originalist_reading, base_extractiveness, 2025, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1982, us_constitution_text__originalist_reading, suppression_requirement, 1982, 0.35).
narrative_ontology:measurement(us_c_su_t1991, us_constitution_text__originalist_reading, suppression_requirement, 1991, 0.42).
narrative_ontology:measurement(us_c_su_t2000, us_constitution_text__originalist_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(us_c_su_t2008, us_constitution_text__originalist_reading, suppression_requirement, 2008, 0.58).
narrative_ontology:measurement(us_c_su_t2016, us_constitution_text__originalist_reading, suppression_requirement, 2016, 0.66).
narrative_ontology:measurement(us_c_su_t2022, us_constitution_text__originalist_reading, suppression_requirement, 2022, 0.72).
narrative_ontology:measurement(us_c_su_t2025, us_constitution_text__originalist_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_text__originalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, us_constitution_text__living_constitutionalist_reading).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, us_constitution_text__positivist_reading).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, second_amendment_text_history_tradition_test).
narrative_ontology:affects_constraint(us_constitution_text__originalist_reading, unenumerated_rights_historical_analogue_gate).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the natural-language label 'constitutional interpretation' covers three structurally distinct readings of one kernel (us_constitution_text). The originalist reading (this file: meaning fixed at ratification; substantial extraction, conservative-legal-movement beneficiary, non-historical claimants as payers), the living-constitutionalist reading (meaning evolves; different beneficiary/victim inversion), and the positivist reading (validity from enactment; orthogonal to both) have different epsilon values, different failure modes, and different research and litigation communities. They are modeled as three linked stories, not one story with a methodology parameter. The upstream sibling edges record that each reading's rise shifts the legitimacy conditions of the others; the two doctrinal downstream edges record that this reading's historical test now governs adjacent doctrine (armed rights, unenumerated-rights gatekeeping), so contamination propagates from this file into those.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_constitution_text__originalist_reading, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
