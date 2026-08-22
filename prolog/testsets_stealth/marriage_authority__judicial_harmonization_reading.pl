% ============================================================================
% CONSTRAINT STORY: marriage_authority__judicial_harmonization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__judicial_harmonization_reading, []).

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
 *   constraint_id: marriage_authority__judicial_harmonization_reading
 *   human_readable: Judicial Harmonization Pathway: Constitutional Floor over Personal Law Codes Without Uniform Codification
 *   domain: legal_pluralism/constitutional_law/comparative_family_law
 *
 * SUMMARY:
 *   In a constitutionally committed but legally plural state, marriage and
 *   family authority is split across community-specific personal law codes.
 *   Rather than enacting a uniform civil code — a directive that has sat
 *   unfulfilled for the republic's entire life — the apex court extends a
 *   constitutional floor of equality and dignity into each code case by case,
 *   striking or reading down individual provisions while leaving the plural
 *   architecture standing. The manifest's expected structural delta
 *   hypothesized a scaffold with moderate extraction; I diverge deliberately.
 *   The pathway declares no sunset clause, and its own justification is the
 *   method (convergence without legislation) rather than a transition to
 *   anything — the steady state IS perpetual incremental adjustment, since
 *   completed convergence would dissolve the court's role and the court shows
 *   no appetite for declaring any domain finished. With genuine coordination,
 *   asymmetric authority transfer, and active enforcement all present, I
 *   author claimed_type tangled_rope and let the engine compute per-seat
 *   types from the structural data. Claim and metrics are independent
 *   authored facts.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__judicial_harmonization_reading, 0.62).
domain_priors:suppression_score(marriage_authority__judicial_harmonization_reading, 0.55).
domain_priors:theater_ratio(marriage_authority__judicial_harmonization_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__judicial_harmonization_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__judicial_harmonization_reading, "Judicial Harmonization Pathway: Constitutional Floor over Personal Law Codes Without Uniform Codification").
narrative_ontology:topic_domain(marriage_authority__judicial_harmonization_reading, "legal_pluralism/constitutional_law/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__judicial_harmonization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__judicial_harmonization_reading, '7d3b6db9-79c4-4513-99dc-21fbe4d2a443').
narrative_ontology:cs_kernel_codification('7d3b6db9-79c4-4513-99dc-21fbe4d2a443', fixed_text).
narrative_ontology:cs_authority_grounding('7d3b6db9-79c4-4513-99dc-21fbe4d2a443', lineage).
narrative_ontology:cs_interpretation_layer_present('7d3b6db9-79c4-4513-99dc-21fbe4d2a443').
narrative_ontology:cs_reading_relation('7d3b6db9-79c4-4513-99dc-21fbe4d2a443', marriage_authority__communal_autonomy_reading, influences).
narrative_ontology:cs_reading_relation('7d3b6db9-79c4-4513-99dc-21fbe4d2a443', marriage_authority__secularist_reading, coexists_with).
narrative_ontology:cs_reading_relation('7d3b6db9-79c4-4513-99dc-21fbe4d2a443', marriage_authority__gender_rights_reading, influences).
narrative_ontology:cs_reading_relation('7d3b6db9-79c4-4513-99dc-21fbe4d2a443', marriage_authority__federalist_millet_reading, coexists_with).
narrative_ontology:cs_axiom('7d3b6db9-79c4-4513-99dc-21fbe4d2a443', foundational, constitutional_floor_binding_on_personal_law).
narrative_ontology:cs_axiom_status(constitutional_floor_binding_on_personal_law, holdable).
narrative_ontology:cs_axiom_grounding('7d3b6db9-79c4-4513-99dc-21fbe4d2a443', constitutional_floor_binding_on_personal_law, conventional).
narrative_ontology:cs_axiom('7d3b6db9-79c4-4513-99dc-21fbe4d2a443', foundational, adjudicated_convergence_without_codification).
narrative_ontology:cs_axiom_status(adjudicated_convergence_without_codification, holdable).
narrative_ontology:cs_axiom_grounding('7d3b6db9-79c4-4513-99dc-21fbe4d2a443', adjudicated_convergence_without_codification, instrumental).
narrative_ontology:cs_reference_frame('7d3b6db9-79c4-4513-99dc-21fbe4d2a443', adjudicated_constitutional_floor_pluralism).
narrative_ontology:cs_drift_state('7d3b6db9-79c4-4513-99dc-21fbe4d2a443', contemporary_post_triple_talaq_docket, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7d3b6db9-79c4-4513-99dc-21fbe4d2a443', '').
narrative_ontology:cs_kernel_id(marriage_authority__judicial_harmonization_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, supreme_court_judiciary).
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, reform_litigants).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, religious_community_institutions).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, parliamentary_legislature).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, sub_floor_community_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hears petitions challenging specific marriage, divorce, maintenance, and succession provisions across the parallel personal law codes, decides which fall short of constitutional guarantees of equality and dignity, and issues binding precedents that lower courts and community forums must follow. Each accepted petition enlarges the body of doctrine the bench itself administers and interprets. Stepping back from the role would require the institution to concede that fundamental-rights enforcement is incomplete without it; its legitimacy narrative is fused with being the guarantor of those rights, so retreat is not realistically available from inside its own self-understanding.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, supreme_court_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__judicial_harmonization_reading, supreme_court_judiciary, beneficiary).

% Maintain parallel family law codes governing marriage, divorce, maintenance, and succession for their members. Each adverse ruling removes one provision from their care while leaving the remainder nominally theirs, so their normative jurisdiction shrinks case by case without any single decisive defeat. They respond through litigation, political mobilization, and pressure on the legislature; they cannot opt out of the constitutional order, and their avenues of success run through persuading the very bench that rules against them.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, religious_community_institutions, payer,
    organized, generational, constrained, national).

% Holds formal power to enact a uniform family code that would supersede the entire judicial pathway in one act, and successive governments have introduced, promised, or commissioned such legislation. None has been carried through: the electoral cost of confronting consolidated community opposition has defeated every attempt, so the house sustains the arrangement by declining to act while formally retaining the power to end it at any session.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, parliamentary_legislature, payer,
    institutional, biographical, constrained, national).

% Individuals — disproportionately women challenging divorce, maintenance, or succession rules — bring the cases that produce each new floor ruling. Relief reaches them and similarly situated others only after years of appeals they must fund and personally sustain; there is no alternative route, since the legislature has not acted and community forums continue applying the unreformed rules in the meantime.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, reform_litigants, beneficiary,
    moderate, biographical, constrained, national).

% Live under the provisions no ruling has yet reached, which in some codes still permit unilateral divorce, unequal succession shares, or denied maintenance. Their exposure ends only when someone else's case arrives at the apex court; they cannot accelerate the queue, and the secular civil-marriage statute that would let some of them opt out carries heavy social cost and does not cover every matter at issue.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, sub_floor_community_members, payer,
    powerless, biographical, trapped, national).

% Traditional interpreters — theologians, customary councils, denominational tribunals — whose readings once defined valid practice within their communities. They appear in the process only as responding parties or amici; no ruling delegates floor-definition to them, and each adverse precedent narrows the space in which their interpretations bind anyone.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, communal_norm_authorities, excluded,
    organized, generational, trapped, national).

% Track the pathway alongside parallel arrangements in other plural legal systems — millet successors, customary-law jurisdictions, concordat regimes — and publish assessments of whether adjudicated convergence protects or erodes community autonomy. They hold no vote; their analyses circulate among benches, law ministries, and academies.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, comparative_family_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__judicial_harmonization_reading, supreme_court_judiciary).
narrative_ontology:fixing_cost_class(marriage_authority__judicial_harmonization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single authoritative interpreter for how constitutional guarantees of equality and dignity apply across several parallel family law systems, letting reform proceed in politically absorbable increments instead of one confrontational codification act, and giving lower courts and community forums a predictable rule of recognition for which provisions remain operative.
% TRANSFER_FUNCTION: Moves norm-defining authority over marriage and family life from community institutions and the formally competent legislature to the apex court, and moves enforceable rights to individual litigants able to sustain multi-year appeals; the costs of litigation and of interim uncertainty fall on those seeking relief and on members still living under unreformed provisions.
% ABSENT_VOICES: Communal norm authorities attend only as respondents; community members who want reform but cannot fund a decade of appeals are represented only vicariously by whichever litigant's facts reach the bench; the legislature speaks through sustained silence. A floor defined without their participation reflects the priorities of whoever happens to litigate.
% DISAPPEARANCE_RATIONALE: Overnight removal would restore every struck-down provision — unilateral divorce, barred maintenance, unequal succession — return norm-authority wholesale to community institutions, and shift all reform energy into a legislative politics that has failed to act for decades; protections recently won for divorced women and daughters would lapse until re-won case by case or code by code.
% FOUNDING_PROBLEM: The founding constitutional settlement guaranteed both religious freedom in personal matters and equal citizenship, leaving family law split across community codes; the pathway accreted to answer how constitutional guarantees could reach inside personal law without the majoritarian confrontation of an imposed uniform code.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the bench: a national Law Commission consultation paper concluded a uniform code was neither necessary nor desirable at the present stage while affirming the underlying rights-versus-pluralism tension; fresh petitions on polygamy and succession parity continue to arrive on the docket; community organizations on every side, and comparative-family-law scholarship, all attest the settlement remains unsettled. No participant claims the founding tension is resolved.
narrative_ontology:disappearance_verdict(marriage_authority__judicial_harmonization_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__judicial_harmonization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__judicial_harmonization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority__judicial_harmonization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__judicial_harmonization_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__judicial_harmonization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__judicial_harmonization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__judicial_harmonization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is substantial but bounded: the transferred good is norm-authority rather than material rent, the floor genuinely delivers protections, and the court's gain is institutional standing rather than captured revenue. Suppression (0.55) is structural, not internalized: constitutional supremacy removes the community veto over its own family law, and the pathway freezes the legislative alternative — any uniform-code bill now detonates against decades of accumulated precedent, so the arrangement suppresses its own replacement. Accessibility collapse is moderate (0.45) because the formal alternatives (codification, communal autonomy, judicial withdrawal) remain conceptually open even though each is politically or doctrinally blocked. Resistance (0.60) is real and occasionally victorious: the earliest landmark maintenance ruling was legislatively reversed within a year by a statute tailored to undo it, and community mobilization has shaped, stalled, and redirected the pathway throughout. Theater (0.32) grows over the interval: the court periodically exhorts the legislature to enact the very code whose absence is the precondition of its own role — a ritual that launders institutional aggrandizement as deference. Enforcement capacity hardened visibly after the triple-talaq ruling struck a personal law practice down as constitutionally void rather than merely voidable, which the rising suppression_requirement series records. The series share one seven-point grid; the early legislative reversal registers as a local perturbation inside a rising trend rather than a full oscillation, so I did not force a cycle-resolving denser grid.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the bench, the pathway is constitutional fidelity — the only way rights can mean anything inside personal law. From the community institutions, the same sequence is piecemeal dispossession: jurisdiction lost provision by provision, with no forum where their interpretive tradition counts as co-author rather than respondent. From the legislature, it is an abdicated competence it cannot reclaim without paying an electoral price no government has dared. From sub-floor members, it is justice on a drip: real rights, arriving decades late, funded by strangers' lawsuits. The engine computes these divergent classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   The bench sits nearest the beneficiary pole: it collects the transferred authority and controls the rules of collection, and its identity-lock (institutional self-concept fused with rights guardianship) places it deeper inside the arrangement than mere preference would. Reform litigants benefit secondarily — they receive enforceable rights — but pay heavily in litigation cost and time, holding them nearer symmetric than pure beneficiary. Community institutions and sub-floor members sit near the target pole: the former lose jurisdiction with constrained recourse, the latter bear the delay with no exit at all. The legislature is the anomalous seat: derivation from its victim declaration would place it near the full-target end, but its position is partly self-chosen — it retains the unilateral power to end the arrangement by enacting a code, and sustains the pathway by declining. I therefore did not author a directionality override: overrides key on the power atom, and the bench and legislature share the institutional atom, so any override correcting one would corrupt the other. The differentiation is carried by the structural declarations and this commentary instead.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — reconciling equal citizenship with personal-law pluralism — is live, corroborated by continuing docket inflow and by a Law Commission process that examined and declined the codification route from outside the benefiting parties. No mandatrophy is declared. The classification guards against two symmetrical mislabels: reading the pathway as pure coordination (rope) would erase the identifiable losers — communities dispossessed provision by provision, members stranded under unreformed rules — while reading it as pure extraction (snare) would erase the genuine collective-action function, since the alternatives (majoritarian codification or abandonment of rights enforcement) are worse for nearly everyone including the losers. The forward risk is piton-shaped rather than snare-shaped: if convergence ever substantially completes and the docket thins, continued supervision would become performance — periodic restatements of a floor nobody contests — and the theater_ratio series would be the leading indicator.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    marriage_authority_kernel_reading_contest,
    'This constraint instantiates the judicial_harmonization_reading of the marriage_authority kernel; would adopting a sibling reading — communal_autonomy, secularist, gender_rights, or federalist_millet — restructure the beneficiary and victim sets so thoroughly that this story''s classification does not survive?',
    'Track doctrinal development against each sibling''s predictions: if floor-rulings begin delegating definition back to community forums, communal_autonomy gains; if a code is enacted, secularist absorbs the field; if equality claimants displace the bench as the pathway''s center, gender_rights does.',
    'Under communal_autonomy or federalist_millet adoption the beneficiary/victim structure inverts — restraint becomes the benefit and adjudicated intervention the imposition — flipping the classification toward enforced extraction from communities; under secularist adoption the pathway reads as a stalling device, raising theater and lowering coordination credit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marriage_authority_kernel_reading_contest, conceptual, 'Kernel-level framing uncertainty: which reading of marriage_authority this mechanism actually serves.').

omega_variable(
    transitional_vs_steady_state_pathway,
    'Is the convergence-without-legislation pathway transitional support that dissolves at completed convergence, or a self-perpetuating steady state whose continuation the bench has an interest in maintaining?',
    'Observe whether the court ever declares a domain fully harmonized and declines further review of it, or whether the docket regenerates indefinitely as new fact patterns and newly contested provisions arrive.',
    'If credibly transitional, the story trends scaffold and the missing sunset clause becomes the central defect; if self-perpetuating, the arrangement is an entrenched tangled_rope carrying long-run piton risk once its coordinating function atrophies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transitional_vs_steady_state_pathway, empirical, 'Whether the pathway has an endpoint or manufactures its own continuation.').

omega_variable(
    judiciary_beneficiary_or_burden_bearer,
    'Does the bench accumulate authority through the pathway, or is it a reluctant conscript performing work the legislature abandoned and periodically begging to be relieved of?',
    'Compare rhetoric with docket behavior: the bench repeatedly urges codification in obiter, yet expands rather than contracts its family-law review when openings appear; institutional-choice analysis of which cases it accepts, and whether it engineers openings, would discriminate the two accounts.',
    'If the bench is a burden-bearer, measured extraction drops materially and the classification trends toward rope or scaffold with the legislature as the responsible agent; if it is a collector, the current metrics stand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judiciary_beneficiary_or_burden_bearer, empirical, 'Whether the primary beneficiary seat is collecting or conscripted.').

omega_variable(
    gradualism_delay_welfare_effect,
    'Does case-by-case pacing protect the reform itself by keeping each step politically absorbable, or does the delay itself take from sub-floor members who spend decades under provisions the constitution forbids?',
    'Cross-jurisdiction comparison of adjudicated versus legislated reform paths: rights-delivery timelines, backlash incidence, and welfare outcomes for the affected populations under each mode.',
    'If delay is net-harmful, effective extraction on the powerless seat rises and the pathway''s coordination credit falls; if gradualism is genuinely protective, the measured extraction overstates the burden and the classification softens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gradualism_delay_welfare_effect, empirical, 'Whether the drip-feed pace is a shield or a toll.').

omega_variable(
    cs_framing_underdetermination,
    'Is the declared commitment-system framing correct — a fixed-text kernel (the constitutional family-law provisions) with lineage authority vested in the bench as designated interpreter — or does the operative kernel sit one layer up, in the basic-structure legitimacy claim the bench layers above the text, or in the docket-dependence that sustains its interpretive authority?',
    'Counterfactual probes: would the floor-doctrine survive a textual amendment touching family law (testing text-as-kernel), and would the bench''s authority survive a collapsed family-law docket (testing docket-dependence)?',
    'If docket-dependence dominates, authority_grounding shifts toward extraction and the drift picture darkens; if the basic-structure layer is the true kernel, the reading looks less like transmission of a founding text and more like self-authored doctrine wearing its mantle.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Which layer of the commitment system is the actual kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__judicial_harmonization_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__judicial_harmonization_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(marr_tr_t7, marriage_authority__judicial_harmonization_reading, theater_ratio, 7, 0.18).
narrative_ontology:measurement(marr_tr_t14, marriage_authority__judicial_harmonization_reading, theater_ratio, 14, 0.22).
narrative_ontology:measurement(marr_tr_t21, marriage_authority__judicial_harmonization_reading, theater_ratio, 21, 0.26).
narrative_ontology:measurement(marr_tr_t28, marriage_authority__judicial_harmonization_reading, theater_ratio, 28, 0.29).
narrative_ontology:measurement(marr_tr_t34, marriage_authority__judicial_harmonization_reading, theater_ratio, 34, 0.31).
narrative_ontology:measurement(marr_tr_t40, marriage_authority__judicial_harmonization_reading, theater_ratio, 40, 0.32).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__judicial_harmonization_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(marr_be_t7, marriage_authority__judicial_harmonization_reading, base_extractiveness, 7, 0.46).
narrative_ontology:measurement(marr_be_t14, marriage_authority__judicial_harmonization_reading, base_extractiveness, 14, 0.5).
narrative_ontology:measurement(marr_be_t21, marriage_authority__judicial_harmonization_reading, base_extractiveness, 21, 0.54).
narrative_ontology:measurement(marr_be_t28, marriage_authority__judicial_harmonization_reading, base_extractiveness, 28, 0.57).
narrative_ontology:measurement(marr_be_t34, marriage_authority__judicial_harmonization_reading, base_extractiveness, 34, 0.6).
narrative_ontology:measurement(marr_be_t40, marriage_authority__judicial_harmonization_reading, base_extractiveness, 40, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__judicial_harmonization_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(marr_su_t7, marriage_authority__judicial_harmonization_reading, suppression_requirement, 7, 0.34).
narrative_ontology:measurement(marr_su_t14, marriage_authority__judicial_harmonization_reading, suppression_requirement, 14, 0.38).
narrative_ontology:measurement(marr_su_t21, marriage_authority__judicial_harmonization_reading, suppression_requirement, 21, 0.43).
narrative_ontology:measurement(marr_su_t28, marriage_authority__judicial_harmonization_reading, suppression_requirement, 28, 0.48).
narrative_ontology:measurement(marr_su_t34, marriage_authority__judicial_harmonization_reading, suppression_requirement, 34, 0.52).
narrative_ontology:measurement(marr_su_t40, marriage_authority__judicial_harmonization_reading, suppression_requirement, 40, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__judicial_harmonization_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, federalist_millet_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'who decides marriage law' decomposes, per the epsilon-invariance principle, into five structurally distinct constraints sharing the marriage_authority kernel: communal_autonomy (state enforces, never authors), secularist (legislature owns the field; pluralism is a transitional anomaly), gender_rights (equality guarantees drive reform from inside), federalist_millet (fragmentation as consociational anti-tyranny), and this judicial_harmonization reading (apex-court floor, no code). Each carries its own epsilon, beneficiary structure, and classification; they are linked here as a constraint family, not averaged into one story. This reading sits downstream of the constitutional-supremacy settlement and upstream of every sibling's operating environment: each floor-ruling shrinks communal autonomy's domain, alters the urgency conditions of the secularist demand, channels which gender-rights claims can travel, and tests the millet claim that fragmentation prevents domination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
