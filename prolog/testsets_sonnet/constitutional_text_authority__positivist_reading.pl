% ============================================================================
% CONSTRAINT STORY: constitutional_text_authority__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text_authority__positivist_reading, []).

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
 *   constraint_id: constitutional_text_authority__positivist_reading
 *   human_readable: Positivist (Rule-of-Recognition) Reading of Constitutional Validity
 *   domain: constitutional_law/legal_theory/interpretive_jurisprudence
 *
 * SUMMARY:
 *   This constraint models the positivist reading of constitutional validity:
 *   a constitutional provision is law because it was enacted through the
 *   recognized formal procedure by an institution with recognized authority
 *   to enact it, full stop — its moral content, however assessed, is
 *   irrelevant to its legal validity (the separability thesis, following
 *   Hart/Kelsen-style positivism). This reading converges with originalism on
 *   the importance of the enactment moment and diverges sharply from living
 *   constitutionalism, which locates authority in evolving moral consensus
 *   rather than procedural pedigree. The reading is administered chiefly by
 *   courts and legislative-drafting institutions who apply and certify the
 *   rule of recognition, and it has real coordination value: it lets legal
 *   actors resolve validity disputes without relitigating first-order moral
 *   questions every time, and it stabilizes expectations about which
 *   enactments count as binding law. But the same procedural formalism that
 *   produces this stability also insulates procedurally regular but
 *   substantively unjust enactments (e.g., provisions ratified through valid
 *   form during periods when whole classes of people were excluded from the
 *   enacting process) from being unsettled on moral grounds — the formal
 *   criterion becomes a shield precisely when moral content would otherwise
 *   disqualify the result.
 *
 * KEY AGENTS:
 *   - constitutional_court_judiciary: administers and certifies the rule of recognition (institutional/arbitrage) — decides what counts as valid enactment
 *   - legislative_drafting_institutions: rely on and benefit from procedural predictability (institutional/mobile)
 *   - administrative_state_officials: implement enactments whose validity is settled procedurally, insulated from having to relitigate moral content (institutional/constrained)
 *   - morally_grounded_rights_claimants: bring challenges premised on substantive injustice that the positivist criterion structurally cannot recognize as invalidating (moderate/constrained)
 *   - marginalized_groups_excluded_at_enactment: bound by provisions ratified through valid form at a time when they had no voice in the enacting process (powerless/trapped)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text_authority__positivist_reading, 0.28).
domain_priors:suppression_score(constitutional_text_authority__positivist_reading, 0.42).
domain_priors:theater_ratio(constitutional_text_authority__positivist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(constitutional_text_authority__positivist_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text_authority__positivist_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text_authority__positivist_reading, "Positivist (Rule-of-Recognition) Reading of Constitutional Validity").
narrative_ontology:topic_domain(constitutional_text_authority__positivist_reading, "constitutional_law/legal_theory/interpretive_jurisprudence").

domain_priors:requires_active_enforcement(constitutional_text_authority__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text_authority__positivist_reading, 'fac75278-9c7e-45e0-aa25-a17c6a17aa11').
narrative_ontology:cs_kernel_codification('fac75278-9c7e-45e0-aa25-a17c6a17aa11', fixed_text).
narrative_ontology:cs_authority_grounding('fac75278-9c7e-45e0-aa25-a17c6a17aa11', practice).
narrative_ontology:cs_interpretation_layer_present('fac75278-9c7e-45e0-aa25-a17c6a17aa11').
narrative_ontology:cs_reading_relation('fac75278-9c7e-45e0-aa25-a17c6a17aa11', constitutional_text_authority__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('fac75278-9c7e-45e0-aa25-a17c6a17aa11', constitutional_text_authority__living_constitutionalist_reading, forecloses).
narrative_ontology:cs_axiom('fac75278-9c7e-45e0-aa25-a17c6a17aa11', foundational, validity_separable_from_moral_content).
narrative_ontology:cs_axiom_status(validity_separable_from_moral_content, holdable).
narrative_ontology:cs_axiom_grounding('fac75278-9c7e-45e0-aa25-a17c6a17aa11', validity_separable_from_moral_content, conventional).
narrative_ontology:cs_axiom('fac75278-9c7e-45e0-aa25-a17c6a17aa11', secondary, rule_of_recognition_determines_law_membership).
narrative_ontology:cs_axiom_status(rule_of_recognition_determines_law_membership, holdable).
narrative_ontology:cs_axiom_grounding('fac75278-9c7e-45e0-aa25-a17c6a17aa11', rule_of_recognition_determines_law_membership, conventional).
narrative_ontology:cs_reference_frame('fac75278-9c7e-45e0-aa25-a17c6a17aa11', hartian_rule_of_recognition_settlement).
narrative_ontology:cs_drift_state('fac75278-9c7e-45e0-aa25-a17c6a17aa11', contemporary_rights_litigation_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('fac75278-9c7e-45e0-aa25-a17c6a17aa11', '').
narrative_ontology:cs_kernel_id(constitutional_text_authority__positivist_reading, constitutional_text_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, constitutional_court_judiciary).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, legislative_drafting_institutions).
narrative_ontology:constraint_beneficiary(constitutional_text_authority__positivist_reading, administrative_state_officials).
narrative_ontology:constraint_victim(constitutional_text_authority__positivist_reading, morally_grounded_rights_claimants).
narrative_ontology:constraint_victim(constitutional_text_authority__positivist_reading, marginalized_groups_excluded_at_enactment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Certifies which enactments satisfy the formal rule of recognition and therefore count as valid law, independent of the enactment's moral content. Administers the doctrine, decides hard procedural cases, and benefits from being perceived as a neutral technical arbiter rather than a moral one — a perception that insulates judicial authority from contestation on substantive grounds.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, constitutional_court_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).

% Draft and enact constitutional provisions relying on the predictability that procedural correctness, not moral controversy, will determine ultimate validity. Benefit from the stability of knowing that properly enacted provisions will not later be invalidated purely on substantive moral grounds.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, legislative_drafting_institutions, beneficiary,
    institutional, generational, mobile, national).

% Implement enactments whose validity has been procedurally settled, freeing them from having to independently adjudicate moral questions before applying the law. Benefit from the efficiency and cover the positivist criterion provides.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, administrative_state_officials, beneficiary,
    organized, biographical, constrained, national).

% Bring challenges to enactments premised on substantive injustice or moral wrongness, and find that the positivist validity criterion structurally cannot recognize such arguments as grounds for invalidation — however compelling the moral claim, it does not bear on the procedural pedigree question. Their only viable path is political: amendment or reinterpretation-by-later-procedure, not judicial invalidation on moral grounds.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, morally_grounded_rights_claimants, payer,
    moderate, biographical, constrained, national).

% Remain bound by constitutional provisions ratified through procedurally valid form at a historical moment when they had no meaningful voice in the enacting process. The positivist criterion treats the procedural pedigree as sufficient for validity regardless of who was excluded from participating in that procedure, leaving them without a validity-based lever to contest provisions whose original enactment process itself embedded their exclusion.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, marginalized_groups_excluded_at_enactment, payer,
    powerless, generational, trapped, national).

% Argue from outside the positivist framework that moral content must bear on legal validity, or that constitutional meaning must evolve with moral understanding. Their theoretical objections shape academic and some judicial discourse but do not control how validity is actually adjudicated in the positivist-dominant institutional mainstream; they are heard but not decisive.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, natural_law_and_living_constitutionalist_theorists, excluded,
    moderate, generational, constrained, national).

% Study how positivist, originalist, and living-constitutionalist validity criteria operate across jurisdictions, documenting where procedural formalism produces morally troubling but 'valid' outcomes and where rival criteria produce different institutional stability profiles.
narrative_ontology:constraint_stakeholder(constitutional_text_authority__positivist_reading, comparative_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text_authority__positivist_reading, constitutional_court_judiciary).
narrative_ontology:fixing_cost_class(constitutional_text_authority__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, mechanically applicable criterion for determining whether a purported constitutional provision is actually valid law, without requiring every dispute to be resolved by relitigating first-order moral or political controversies. This lets courts, legislatures, and citizens share a common baseline about what counts as binding law.
% TRANSFER_FUNCTION: Moves interpretive authority and the power to foreclose moral argument from claimants and excluded groups to the institutions that certify procedural pedigree (primarily courts and drafting bodies) — validity questions that might otherwise be contested on substantive moral grounds are settled instead by reference to institutional form, which systematically favors whoever controlled the enactment process.
% ABSENT_VOICES: Groups excluded from the original enacting process (by suffrage restriction, colonial status, or other historical exclusion) have no seat in either the original procedural pedigree or in most subsequent challenges, since the positivist criterion does not treat their absence from enactment as a validity defect so long as the formal procedure recognized at the time was followed. Natural-law theorists and living-constitutionalist judges object in principle but do not control the doctrine's operation in positivist-dominant systems.
% DISAPPEARANCE_RATIONALE: If the positivist criterion vanished as the operative validity test, courts would lose a stable, low-cost method for resolving validity disputes and would have to adjudicate every contested enactment by reference to either historical original meaning or contemporary moral consensus — dramatically increasing the stakes and unpredictability of constitutional litigation, reopening previously 'settled' provisions to moral challenge, and shifting real power from institutions that currently benefit from procedural insulation toward whichever rival criterion (originalist or living-constitutionalist) filled the vacuum.
% FOUNDING_PROBLEM: Nineteenth and twentieth-century legal theory needed to distinguish law from morality and from raw political power in order to explain why legal systems can identify valid law without each judge independently re-deriving justice from first principles — the positivist project (Bentham, Austin, Kelsen, Hart) answered a genuine theoretical problem: what makes a rule LAW rather than merely a moral claim or a command backed by force.
% FOUNDING_PROBLEM_CORROBORATION: Legal philosophers working outside constitutional adjudication (H.L.A. Hart's successors, critical legal studies scholars, comparative constitutionalists) attest that the theoretical problem of distinguishing law from morality remains live and unresolved in the abstract. But critical legal scholars and rights-claimant advocates — outside the beneficiary set of courts and drafting institutions — argue the doctrine's PRACTICAL function has shifted from solving that theoretical problem toward shielding specific procedurally-valid-but-substantively-contested enactments from moral reassessment, particularly enactments concluded before excluded groups had enacting power. No source entirely outside legal academia and legal practice independently corroborates either the 'still solving the founding problem' or the 'now mostly shielding outcomes' account; both readings come from within the legal-theoretical community, just from different factions within it.
narrative_ontology:disappearance_verdict(constitutional_text_authority__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text_authority__positivist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text_authority__positivist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(constitutional_text_authority__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text_authority__positivist_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text_authority__positivist_reading_tests).
:- end_tests(constitutional_text_authority__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.28) because the positivist criterion genuinely does most of its work as a coordination device — it resolves validity disputes cheaply and predictably rather than functioning primarily as a rent-extraction mechanism. Suppression sits higher (0.42) because the doctrine actively forecloses one entire category of legal argument (moral-content challenges to validity) regardless of the argument's substantive force — that foreclosure is a real, if narrow, suppressive act, not merely descriptive. Theater ratio is low-moderate and rising (0.10 to 0.22) as courts increasingly perform 'pure' procedural analysis in cases where the procedural question is genuinely contested and moral reasoning visibly leaks through the formal vocabulary. Accessibility collapse is moderate (0.58): once a court certifies procedural validity, moral re-litigation is largely closed off, though not permanently — later amendment or repeal remains available. Resistance is moderate (0.35), coming from rights claimants and legal theorists (natural-law and living-constitutionalist scholars) who contest the separability thesis itself.
 *
 * PERSPECTIVAL GAP:
 *   From the judiciary's seat, the positivist criterion is a rope: a genuine, low-overhead coordination solution that lets legal disputes about validity be resolved without endless moral relitigation, benefiting everyone who needs predictable law. From the seat of a claimant whose substantive grievance is procedurally unanswerable — no matter how compelling the moral argument — the same criterion operates as an enforced wall: an active refusal to hear the claim on its own terms, sustained by institutional authority that has an interest in maintaining that wall. Both readings are structurally accurate from their respective positions; the tangled_rope classification holds both together rather than picking one.
 *
 * DIRECTIONALITY LOGIC:
 *   Courts and drafting institutions are declared beneficiaries: they administer the rule of recognition and gain from being seen as neutral appliers of form rather than as moral arbiters, which insulates their authority from moral contestation. Administrative officials benefit secondarily by having settled validity questions to implement rather than relitigate. The victims are those for whom the moral/procedural gap actually bites: claimants pressing substantive injustice claims that the positivist criterion structurally cannot hear as validity challenges, and groups who had no voice in the original enacting process but are nonetheless bound by its procedurally valid product. Their exit options are genuinely constrained-to-trapped: amendment processes exist but require exactly the institutional access and numerical power the excluded groups characteristically lack.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem the positivist criterion was built to solve — resolving jurisdictional and validity disputes without relitigating first-order moral controversies at every turn — remains substantially live: legal systems still need a workable answer to 'is this actually law' that does not collapse into permanent moral debate. But the criterion's persistence in cases where procedurally valid enactments produce results widely regarded as unjust suggests partial mandatrophy: the coordination function (settling validity efficiently) has been joined by a protective function (insulating certain historical or politically difficult outcomes from moral reassessment) that was not part of the original justification. Classifying this as tangled_rope rather than snare or mountain prevents both errors: treating the doctrine as pure extraction would miss its genuine, still-functioning coordination value; treating it as an inevitable natural fact (mountain) would miss that it is a chosen jurisprudential commitment with identifiable beneficiaries and victims, defended actively against rival theories.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_positivist_variant,
    'This constraint instantiates the positivist reading of the constitutional_text_authority kernel — validity flows from a rule of recognition (formal enactment pedigree) rather than moral content or original semantic meaning or evolving values. What would the originalist and living-constitutionalist siblings change structurally?',
    'Compare against constraint_originalist_reading and constraint_living_constitutionalist_reading: the originalist reading substitutes fixed historical semantic content for procedural pedigree as the validity criterion; the living-constitutionalist reading substitutes contemporary moral consensus. The disagreement is located at the validity criterion itself — what makes a constitutional provision LAW — not at any shared textual surface.',
    'Under the positivist reading, a procedurally valid but morally repugnant enactment remains valid law (separability thesis); under living constitutionalism the same enactment could be reinterpreted or its application could shift with evolving values; under originalism it is bound in either case to the ratification-era understanding regardless of procedure quality. These produce different victim sets: positivism protects procedurally regular but substantively unjust enactments (e.g., historical exclusions ratified through proper form) from reinterpretation on moral grounds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_positivist_variant, conceptual, 'This story is one reading (positivist) of the constitutional_text_authority kernel; siblings are separate constraints.').

omega_variable(
    separability_thesis_stability,
    'Is the law/morality separability thesis itself a stable, mechanically applicable rule, or does ''formal enactment procedure'' quietly smuggle in moral judgments at the margins (e.g., what counts as a valid quorum, a valid signature, a valid amendment process during crisis conditions)?',
    'Examine hard cases where procedural validity is contested (coups that observe forms, emergency-power enactments, judicially invalidated but never repealed provisions) and check whether courts applying the positivist test in fact import substantive moral reasoning under the label of ''procedural'' analysis.',
    'If procedural analysis is never morality-free in practice, the positivist reading''s claimed independence from moral content is itself a false summit — a mountain-framed claim (mechanical rule-following) with identifiable institutional beneficiaries (courts and drafters who benefit from being seen as neutral appliers of form rather than moral arbiters).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(separability_thesis_stability, conceptual, 'Whether the positivist separability claim survives contact with contested procedural hard cases.').

omega_variable(
    beneficiary_capture_of_neutrality_claim,
    'Do the institutions that administer the rule of recognition (courts, legislatures, agencies) benefit from the positivist framing in a way that would not survive if the framing were understood as institution-preserving rather than morally neutral?',
    'Compare outcomes when procedurally valid enactments produce results institutions themselves find embarrassing (e.g., laws enacted through proper form but later widely regarded as unjust) — does the institution defend positivist validity to protect its own authority-continuity, or does it reach for moral/living-constitutionalist reasoning when convenient?',
    'If institutions selectively invoke positivism to shield outcomes and living-constitutionalism to revise others, positivism functions partly as a tool of institutional self-protection rather than a neutral metatheory — supporting the tangled_rope classification over a pure rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_of_neutrality_claim, empirical, 'Whether institutional self-interest, not neutral theory, drives selective invocation of the positivist criterion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text_authority__positivist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text_authority__positivist_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cons_tr_t8, constitutional_text_authority__positivist_reading, theater_ratio, 8, 0.13).
narrative_ontology:measurement(cons_tr_t16, constitutional_text_authority__positivist_reading, theater_ratio, 16, 0.16).
narrative_ontology:measurement(cons_tr_t24, constitutional_text_authority__positivist_reading, theater_ratio, 24, 0.19).
narrative_ontology:measurement(cons_tr_t32, constitutional_text_authority__positivist_reading, theater_ratio, 32, 0.21).
narrative_ontology:measurement(cons_tr_t40, constitutional_text_authority__positivist_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text_authority__positivist_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(cons_be_t8, constitutional_text_authority__positivist_reading, base_extractiveness, 8, 0.21).
narrative_ontology:measurement(cons_be_t16, constitutional_text_authority__positivist_reading, base_extractiveness, 16, 0.24).
narrative_ontology:measurement(cons_be_t24, constitutional_text_authority__positivist_reading, base_extractiveness, 24, 0.26).
narrative_ontology:measurement(cons_be_t32, constitutional_text_authority__positivist_reading, base_extractiveness, 32, 0.27).
narrative_ontology:measurement(cons_be_t40, constitutional_text_authority__positivist_reading, base_extractiveness, 40, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text_authority__positivist_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(cons_su_t8, constitutional_text_authority__positivist_reading, suppression_requirement, 8, 0.33).
narrative_ontology:measurement(cons_su_t16, constitutional_text_authority__positivist_reading, suppression_requirement, 16, 0.36).
narrative_ontology:measurement(cons_su_t24, constitutional_text_authority__positivist_reading, suppression_requirement, 24, 0.39).
narrative_ontology:measurement(cons_su_t32, constitutional_text_authority__positivist_reading, suppression_requirement, 32, 0.41).
narrative_ontology:measurement(cons_su_t40, constitutional_text_authority__positivist_reading, suppression_requirement, 40, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text_authority__positivist_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(constitutional_text_authority__positivist_reading, 0.12).
narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, constitutional_originalist_reading).
narrative_ontology:affects_constraint(constitutional_text_authority__positivist_reading, constitutional_living_constitutionalist_reading).

% DUAL FORMULATION NOTE:
% Constitutional_text_authority kernel family (3 readings): positivist_reading (this story, tangled_rope) locates validity in procedural pedigree; originalist_reading locates validity in fixed historical semantic meaning; living_constitutionalist_reading locates validity in evolving contemporary moral consensus. All three readings share the same underlying text and institutional apparatus but instantiate different validity criteria with different beneficiary/victim structures. This story's beneficiaries (courts, drafting institutions) partially overlap with originalism's beneficiaries (also courts, but historians/textualist scholars rather than drafters) but diverge sharply from living constitutionalism's beneficiaries (reform-oriented judiciary, contemporary rights movements). Each reading must be evaluated as its own constraint per the ε-invariance principle; do not average across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
