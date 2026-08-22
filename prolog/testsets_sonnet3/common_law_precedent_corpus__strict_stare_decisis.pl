% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__strict_stare_decisis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__strict_stare_decisis, []).

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
 *   constraint_id: common_law_precedent_corpus__strict_stare_decisis
 *   human_readable: Strict Stare Decisis: Precedent as Rigid Backward Constraint
 *   domain: legal_theory/jurisprudence/constitutional_law
 *
 * SUMMARY:
 *   This story instantiates the strict stare decisis reading of the
 *   common-law precedent kernel: precedent binds as a genuinely
 *   backward-looking constraint, and departure requires an extraordinary
 *   showing beyond ordinary disagreement with the reasoning or outcome. This
 *   is one of three structurally distinct readings of how precedent functions
 *   in common-law systems; the evolutionary_framework reading (precedent as
 *   adaptive scaffold permitting reinterpretation) and the
 *   pluralist_balancing reading (precedent weight varies by domain, balanced
 *   case-by-case) are separate constraints with their own epsilon values, not
 *   alternative measurements of this one. Under this reading's own lights,
 *   the standing arrangement is the high-rigidity doctrine actually practiced
 *   by courts that treat overruling as exceptional; epsilon is authored for
 *   that arrangement, not for what a looser doctrine would look like.
 *
 * KEY AGENTS:
 *   - settled_commercial_interests: primary beneficiary (powerful/mobile) — extracts predictability value from doctrinal rigidity
 *   - appellate_judiciary_institutional_legitimacy: agenda-setter (institutional/analytical) — administers and enforces the extraordinary-justification threshold
 *   - litigants_with_novel_claims: primary target (moderate/constrained) — bears the cost of the high bar for doctrinal correction
 *   - historically_marginalized_claimants: structural victim (powerless/trapped) — bears the accumulated weight of exclusionary-era holdings
 *   - legal_academics_and_court_watchers: analytical observer — tracks overruling patterns without formal power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, 0.52).
domain_priors:suppression_score(common_law_precedent_corpus__strict_stare_decisis, 0.61).
domain_priors:theater_ratio(common_law_precedent_corpus__strict_stare_decisis, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, extractiveness, 0.52).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0.61).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__strict_stare_decisis, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__strict_stare_decisis, "Strict Stare Decisis: Precedent as Rigid Backward Constraint").
narrative_ontology:topic_domain(common_law_precedent_corpus__strict_stare_decisis, "legal_theory/jurisprudence/constitutional_law").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__strict_stare_decisis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__strict_stare_decisis, 'fc28d572-f680-4203-b449-e067cf38b07d').
narrative_ontology:cs_kernel_codification('fc28d572-f680-4203-b449-e067cf38b07d', distributed).
narrative_ontology:cs_authority_grounding('fc28d572-f680-4203-b449-e067cf38b07d', lineage).
narrative_ontology:cs_interpretation_layer_present('fc28d572-f680-4203-b449-e067cf38b07d').
narrative_ontology:cs_reading_relation('fc28d572-f680-4203-b449-e067cf38b07d', common_law_precedent_corpus__evolutionary_framework, coexists_with).
narrative_ontology:cs_reading_relation('fc28d572-f680-4203-b449-e067cf38b07d', common_law_precedent_corpus__pluralist_balancing, influences).
narrative_ontology:cs_axiom('fc28d572-f680-4203-b449-e067cf38b07d', foundational, predictability_outweighs_case_specific_correction).
narrative_ontology:cs_axiom_status(predictability_outweighs_case_specific_correction, holdable).
narrative_ontology:cs_axiom_grounding('fc28d572-f680-4203-b449-e067cf38b07d', predictability_outweighs_case_specific_correction, instrumental).
narrative_ontology:cs_axiom('fc28d572-f680-4203-b449-e067cf38b07d', foundational, departure_requires_showing_beyond_ordinary_disagreement).
narrative_ontology:cs_axiom_status(departure_requires_showing_beyond_ordinary_disagreement, holdable).
narrative_ontology:cs_axiom_grounding('fc28d572-f680-4203-b449-e067cf38b07d', departure_requires_showing_beyond_ordinary_disagreement, conventional).
narrative_ontology:cs_reference_frame('fc28d572-f680-4203-b449-e067cf38b07d', founding_era_predictability_doctrine).
narrative_ontology:cs_drift_state('fc28d572-f680-4203-b449-e067cf38b07d', contemporary_rights_expansion_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fc28d572-f680-4203-b449-e067cf38b07d', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, settled_commercial_interests).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, appellate_judiciary_institutional_legitimacy).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, established_legal_practitioners).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, litigants_with_novel_claims).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, historically_marginalized_claimants).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, lower_court_judges_bound_by_defective_holdings).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__strict_stare_decisis, rule_of_law_predictability_doctrine).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__strict_stare_decisis, judicial_restraint_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Structure contracts, financing, and business practices around the assumption that established precedent will not shift. Benefits directly from the predictability strict stare decisis guarantees; can lobby for legislative fixes if a precedent becomes inconvenient, giving it exit options ordinary litigants lack.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, settled_commercial_interests, beneficiary,
    powerful, generational, mobile, national).

% Sets and enforces the doctrine of extraordinary justification for overruling precedent. Frames strict adherence as necessary to preserve the judiciary's non-political character and public confidence in law as principled rather than personal. Collects institutional legitimacy from appearing bound by something larger than individual judges.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, appellate_judiciary_institutional_legitimacy, agenda_setter,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(common_law_precedent_corpus__strict_stare_decisis, appellate_judiciary_institutional_legitimacy, beneficiary).

% Built careers and firm practices on mastery of existing doctrine. A stable precedent corpus is a professional asset — expertise in settled law retains value only if the law stays settled. Can shift specialties or jurisdictions if doctrine changes, unlike clients bound to a single forum.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, established_legal_practitioners, beneficiary,
    organized, biographical, mobile, national).

% Bring claims that require courts to recognize new harms or extend doctrine to changed circumstances. Face a demand for extraordinary justification before any court will revisit controlling precedent, regardless of how the facts have moved on. Cannot choose a different forum system; must litigate within the jurisdiction whose precedent governs them.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, litigants_with_novel_claims, payer,
    moderate, immediate, constrained, regional).

% Bear the accumulated weight of precedents decided in eras with narrower conceptions of who counted as a rights-holder. Strict stare decisis treats those holdings as presumptively binding absent extraordinary justification, meaning correction of embedded injustice requires overcoming the same high bar as any other doctrinal shift. Have no exit from the legal system that governs their claims.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, historically_marginalized_claimants, payer,
    powerless, generational, trapped, national).

% Must apply controlling precedent even where they can identify its reasoning as outdated or its application as producing an unjust result in the case before them. Their professional obligation to follow vertical stare decisis leaves them almost no room to signal disagreement except through dicta or concurrence, which rarely changes outcomes.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, lower_court_judges_bound_by_defective_holdings, payer,
    moderate, biographical, constrained, regional).

% Can in principle override common-law precedent by statute, but strict stare decisis doctrines about constitutional interpretation place large swaths of precedent beyond ordinary legislative correction, requiring supermajority processes (constitutional amendment) that are rarely achievable. Their institutional voice on constitutional precedent specifically is structurally muted relative to their formal lawmaking power.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, legislatures, excluded,
    institutional, generational, constrained, national).

% Study overruling patterns, write amicus briefs, and track which precedents survive extraordinary-justification review. Have no formal power to bind or unbind precedent but shape the discourse used by parties who do.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, legal_academics_and_court_watchers, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_law_precedent_corpus__strict_stare_decisis, diffuse).
narrative_ontology:fixing_cost_class(common_law_precedent_corpus__strict_stare_decisis, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides predictability: parties can plan conduct, draft contracts, and structure institutions around the expectation that settled legal rules will not shift absent an extraordinary showing, reducing transaction costs and protecting reliance interests across the whole population subject to the law.
% TRANSFER_FUNCTION: Moves the cost of legal error and legal change from those who benefit from stability (settled interests, the judiciary's self-image, incumbent practitioners) onto those who need the law to adapt to their circumstances (novel claimants, marginalized groups carrying the weight of old holdings, lower courts bound to apply them).
% ABSENT_VOICES: Litigants whose claims would require overruling precedent rarely get a hearing on the merits of whether the old rule was ever correct — the extraordinary-justification threshold screens out the argument before it is fully made. Communities harmed by precedents set when they had no voice in the judiciary or the bar have no seat at the table when the doctrine's continued weight is assessed.
% DISAPPEARANCE_RATIONALE: If strict adherence to precedent vanished overnight, courts would revisit settled doctrine far more readily; commercial actors would need to hedge against sudden doctrinal shifts, practitioners' specialized precedent knowledge would depreciate faster, and claimants seeking correction of outdated or unjust holdings would face a dramatically lower bar. Contract drafting, insurance pricing, and litigation strategy across the legal system would reorganize around a higher-volatility doctrinal environment.
% FOUNDING_PROBLEM: Early common-law systems needed a mechanism to prevent each new panel of judges from re-deciding settled questions arbitrarily, which would have made legal outcomes unpredictable and vulnerable to the preferences of whoever happened to be on the bench.
% FOUNDING_PROBLEM_CORROBORATION: Sitting appellate judges and legal historians attest the predictability problem remains live and cite comparative examples from jurisdictions with weaker precedent doctrines showing greater outcome volatility. Civil rights scholars and comparative law academics — outside the judiciary that benefits from the doctrine's legitimacy function — argue the strict version has drifted from solving unpredictability into insulating specific substantive outcomes (particularly around historically exclusionary holdings) from correction, and that a weaker doctrine could preserve predictability while permitting more correction.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__strict_stare_decisis, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__strict_stare_decisis, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__strict_stare_decisis, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(common_law_precedent_corpus__strict_stare_decisis, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__strict_stare_decisis, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__strict_stare_decisis_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_law_precedent_corpus__strict_stare_decisis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52) rather than extreme because the doctrine genuinely solves a real coordination problem — legal predictability has measurable value across the whole population subject to the law, not just to concentrated beneficiaries. Suppression (0.61) exceeds extractiveness because the mechanism by which departure is foreclosed (the extraordinary-justification standard itself) is a strong, actively-enforced barrier independent of how much any given holding actually extracts. Theater ratio stays low-moderate (0.28) because the predictability function is substantially real, not merely performed — though it rises over the measured interval as accumulated precedent increasingly serves institutional self-legitimation rather than fresh coordination value. accessibility_collapse (0.58) reflects that alternative doctrinal paths (distinguishing, narrow construction, legislative override) remain nominally available but are heavily disfavored in practice. resistance (0.55) captures the sustained but only partially successful challenges from novel claimants and reform-minded academics.
 *
 * PERSPECTIVAL GAP:
 *   From the appellate judiciary's seat, strict stare decisis is a principled constraint on judicial willfulness that preserves the rule of law's non-political character. From the seat of a marginalized claimant carrying the weight of an old exclusionary holding, the identical extraordinary-justification standard operates as an enforced barrier to correcting an injustice that the claimant had no voice in creating. The engine computes these as structurally different experiences of the same doctrine from the declared power/exit data — this story does not adjudicate which seat is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   Settled commercial interests and established practitioners sit near the beneficiary end: they hold mobile exit options (can restructure transactions or shift practice areas) and collect the predictability value directly. The appellate judiciary is agenda-setter and secondary beneficiary — it administers the extraordinary-justification threshold and collects institutional legitimacy from appearing constrained by something beyond individual preference, even though it is the body that could relax the standard. Litigants with novel claims and historically marginalized claimants sit near the target end: constrained-to-trapped exit options, no ability to select a different precedent regime, and they bear the transfer directly when the extraordinary bar screens out correction. Lower court judges are a distinctive intermediate case — moderate power but constrained exit, bound to apply holdings they may recognize as defective, extracting no benefit from the arrangement themselves.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification (rather than snare) matters here precisely because strict stare decisis retains a live coordination function — legal predictability is not fictional, and eliminating precedent's binding force entirely would genuinely harm parties who rely on settled expectations. Labeling the whole arrangement pure extraction would mislabel real coordination as capture. But the requires_active_enforcement flag and the identified victim classes prevent the opposite error — treating the extraordinary-justification standard as costless coordination when it also entrenches specific substantive outcomes (particularly precedents from eras with narrower rights recognition) against correction well past the point where the original coordination rationale justifies the specific holding being protected.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is strict stare decisis the descriptively accurate account of how common-law precedent actually functions, or is it itself a contested reading that courts invoke selectively — applying it strictly to protect certain outcomes while quietly practicing evolutionary or pluralist reasoning elsewhere?',
    'Empirical study of overruling rates and the stated justifications across doctrinal areas: consistent extraordinary-justification demands across domains would support the strict reading as the operative account; domain-variable rigor would support pluralist_balancing as more descriptively accurate, with strict_stare_decisis functioning as rhetoric deployed selectively.',
    'If courts practice pluralist balancing while claiming strict stare decisis, this story''s high suppression and accessibility_collapse values would overstate the doctrine''s actual operation in domains where courts are quietly more flexible, and the theater_ratio would need to rise substantially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether strict stare decisis is the operative doctrine or a selectively-invoked framing over an actually pluralist practice.').

omega_variable(
    extraordinary_justification_threshold_stability,
    'Has the practical content of ''extraordinary justification'' itself drifted over time, becoming either more permissive (eroding the rigidity this reading claims) or more restrictive (hardening the doctrine beyond its founding rationale)?',
    'Longitudinal coding of overruling opinions to measure whether the stated bar for departure has stayed constant, loosened, or tightened relative to the doctrine''s articulation a generation earlier.',
    'A drifting threshold would mean the epsilon value authored here reflects only a snapshot of a moving target, and the temporal measurements should be read as tracking a doctrine still in motion rather than a stable equilibrium.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraordinary_justification_threshold_stability, empirical, 'Whether the extraordinary-justification bar itself has hardened or loosened over the measured interval.').

omega_variable(
    legitimacy_versus_predictability_function,
    'Is the beneficiary classification of appellate_judiciary_institutional_legitimacy capturing a genuine coordination benefit (predictable law is a public good the judiciary helps supply) or a self-serving institutional interest (the judiciary benefits from appearing constrained regardless of whether the constraint produces good outcomes)?',
    'Compare judicial behavior in high-visibility, legitimacy-sensitive cases against low-visibility cases: if extraordinary-justification rigor tracks public scrutiny rather than case-specific coordination value, the legitimacy-interest reading is favored.',
    'If the legitimacy-interest reading dominates, the judiciary''s directionality should be pushed further toward the beneficiary end than currently modeled, strengthening the tangled_rope classification''s extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_versus_predictability_function, conceptual, 'Whether judicial adherence to strict precedent serves genuine coordination or institutional self-interest in appearing constrained.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__strict_stare_decisis, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0, 0.15).
narrative_ontology:measurement(comm_tr_t12, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 12, 0.18).
narrative_ontology:measurement(comm_tr_t24, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 24, 0.21).
narrative_ontology:measurement(comm_tr_t36, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 36, 0.24).
narrative_ontology:measurement(comm_tr_t48, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 48, 0.26).
narrative_ontology:measurement(comm_tr_t60, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 60, 0.28).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 0, 0.34).
narrative_ontology:measurement(comm_be_t12, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 12, 0.39).
narrative_ontology:measurement(comm_be_t24, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 24, 0.44).
narrative_ontology:measurement(comm_be_t36, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 36, 0.47).
narrative_ontology:measurement(comm_be_t48, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 48, 0.5).
narrative_ontology:measurement(comm_be_t60, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 60, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(comm_su_t12, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(comm_su_t24, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 24, 0.55).
narrative_ontology:measurement(comm_su_t36, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 36, 0.58).
narrative_ontology:measurement(comm_su_t48, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 48, 0.6).
narrative_ontology:measurement(comm_su_t60, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 60, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__strict_stare_decisis, enforcement_mechanism).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus__evolutionary_framework).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus__pluralist_balancing).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the common_law_precedent_corpus kernel, decomposed per the epsilon-invariance principle because the natural-language label 'stare decisis' covers structurally distinct claims about how binding precedent actually is. strict_stare_decisis carries the highest suppression and accessibility_collapse of the three; evolutionary_framework and pluralist_balancing are separate stories with their own epsilon values, victim/beneficiary structures, and classifications, linked here rather than folded into a single averaged constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
