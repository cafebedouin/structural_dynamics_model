% ============================================================================
% CONSTRAINT STORY: electronic_money_emergence__first_held_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_electronic_money_emergence__first_held_reading, []).

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
 *   constraint_id: electronic_money_emergence__first_held_reading
 *   human_readable: First Institutional Holding of Dematerialized Currency as Emergence Threshold
 *   domain: economic_history/monetary_theory/technology_studies
 *
 * SUMMARY:
 *   This story instantiates the 'first held' reading of the
 *   electronic-money-emergence kernel: digital money is treated as emerging
 *   at a discrete, identifiable institutional event — the moment some
 *   institutional bearer held dematerialized currency in a form legally and
 *   administratively distinguishable from physical notes. This reading ties
 *   emergence to a certifiable threshold (regulatory recognition of a
 *   specific holding), which is what makes it structurally distinct from a
 *   reading that locates emergence in conceptual thinkability (prior to any
 *   institutional measurement) or a reading that treats the entire category
 *   as a retroactive statistical artifact of the M4/M5 aggregate distinction.
 *   Those are different constraints, authored separately, and linked here
 *   only via network edges — this file's ε is computed solely from this
 *   reading's own structure: an administratively useful but contestable
 *   threshold-certification apparatus that quietly privileges the certifying
 *   institutions' own recognized holdings over functionally equivalent but
 *   uncertified prior practice.
 *
 * KEY AGENTS:
 *   - settlement_banks: primary beneficiary (institutional/arbitrage) — certified as the origin point, gains legal/credit privileges from the ledger-entry recognition
 *   - central_bank_ledger_authorities: agenda_setter (institutional/analytical) — certifies which holding counts as the threshold event
 *   - cash_dependent_depositors: primary target (powerless/trapped) — bear friction as policy defaults shift toward the certified dematerialized form
 *   - non_reporting_shadow_intermediaries: secondary target (moderate/constrained) — retroactively excluded from the legitimated category
 *   - informal_ledger_keepers: excluded voice (powerless/trapped) — prior functionally-equivalent holders with no standing in the certification process
 *   - monetary_historians: analytical observer (analytical/analytical) — can destabilize the certified date with archival evidence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(electronic_money_emergence__first_held_reading, 0.28).
domain_priors:suppression_score(electronic_money_emergence__first_held_reading, 0.22).
domain_priors:theater_ratio(electronic_money_emergence__first_held_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(electronic_money_emergence__first_held_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(electronic_money_emergence__first_held_reading, tangled_rope).
narrative_ontology:human_readable(electronic_money_emergence__first_held_reading, "First Institutional Holding of Dematerialized Currency as Emergence Threshold").
narrative_ontology:topic_domain(electronic_money_emergence__first_held_reading, "economic_history/monetary_theory/technology_studies").

domain_priors:requires_active_enforcement(electronic_money_emergence__first_held_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(electronic_money_emergence__first_held_reading, 'fbeb9a04-faf7-47c2-8c6b-b9df090d8f2e').
narrative_ontology:cs_kernel_codification('fbeb9a04-faf7-47c2-8c6b-b9df090d8f2e', formalized).
narrative_ontology:cs_authority_grounding('fbeb9a04-faf7-47c2-8c6b-b9df090d8f2e', extraction).
narrative_ontology:cs_interpretation_layer_present('fbeb9a04-faf7-47c2-8c6b-b9df090d8f2e').
narrative_ontology:cs_reading_relation('fbeb9a04-faf7-47c2-8c6b-b9df090d8f2e', electronic_money_emergence__became_thinkable_reading, coexists_with).
narrative_ontology:cs_reading_relation('fbeb9a04-faf7-47c2-8c6b-b9df090d8f2e', electronic_money_emergence__m4_m5_collapse_reading, influences).
narrative_ontology:cs_axiom('fbeb9a04-faf7-47c2-8c6b-b9df090d8f2e', foundational, institutional_certification_constitutes_emergence).
narrative_ontology:cs_axiom_status(institutional_certification_constitutes_emergence, holdable).
narrative_ontology:cs_axiom_grounding('fbeb9a04-faf7-47c2-8c6b-b9df090d8f2e', institutional_certification_constitutes_emergence, conventional).
narrative_ontology:cs_axiom('fbeb9a04-faf7-47c2-8c6b-b9df090d8f2e', secondary, regulatory_recognition_is_necessary_for_monetary_ontology).
narrative_ontology:cs_axiom_status(regulatory_recognition_is_necessary_for_monetary_ontology, holdable).
narrative_ontology:cs_axiom_grounding('fbeb9a04-faf7-47c2-8c6b-b9df090d8f2e', regulatory_recognition_is_necessary_for_monetary_ontology, instrumental).
narrative_ontology:cs_reference_frame('fbeb9a04-faf7-47c2-8c6b-b9df090d8f2e', physical_currency_legal_tender_baseline).
narrative_ontology:cs_drift_state('fbeb9a04-faf7-47c2-8c6b-b9df090d8f2e', post_electronic_settlement_infrastructure_maturity, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fbeb9a04-faf7-47c2-8c6b-b9df090d8f2e', '').
narrative_ontology:cs_kernel_id(electronic_money_emergence__first_held_reading, electronic_money_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, settlement_banks).
narrative_ontology:constraint_beneficiary(electronic_money_emergence__first_held_reading, central_bank_ledger_authorities).
narrative_ontology:constraint_victim(electronic_money_emergence__first_held_reading, cash_dependent_depositors).
narrative_ontology:constraint_victim(electronic_money_emergence__first_held_reading, non_reporting_shadow_intermediaries).
narrative_ontology:constraint_vindicates(electronic_money_emergence__first_held_reading, legal_tender_recognition_doctrine).
narrative_ontology:constraint_vindicates(electronic_money_emergence__first_held_reading, institutional_custody_as_money_creation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The first institution to hold dematerialized balances as ledger entries rather than physical notes gains the benefit of the regulatory and legal apparatus treating that moment as the origin of a new money form. This retroactively legitimizes internal book-entry balances as money proper, letting settlement banks expand credit against ledger holdings with the same legal status as physical reserves, while the accounting event itself is dated to their institutional practice.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, settlement_banks, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(electronic_money_emergence__first_held_reading, settlement_banks, agenda_setter).

% Defines and certifies the threshold event — the first institutional holder recognized as bearing dematerialized currency — as a matter of regulatory record. This certification power lets the authority decide which ledger entries count as 'money' for statistical and legal purposes, a decision with downstream consequences for reserve requirements, deposit insurance scope, and monetary aggregate reporting.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, central_bank_ledger_authorities, beneficiary,
    institutional, civilizational, analytical, national).
narrative_ontology:stakeholder_secondary_role(electronic_money_emergence__first_held_reading, central_bank_ledger_authorities, agenda_setter).

% Hold physical notes and coin, which after the threshold event are increasingly treated by policy and infrastructure as a legacy form. As institutional recognition of dematerialized currency hardens into legal and administrative default, cash users bear rising friction — fee structures, branch closures, and settlement delays — imposed by an emergence narrative they had no part in setting and cannot contest through any available channel.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, cash_dependent_depositors, payer,
    powerless, biographical, trapped, national).

% Operated informal or semi-formal money transmission before the threshold event was certified. Once an institutional bearer's holding becomes the recognized origin point of 'digital money,' regulatory frameworks retroactively classify comparable but uncertified holdings as non-compliant or illegitimate, forcing these intermediaries into costly formalization or exclusion from the newly bounded category.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, non_reporting_shadow_intermediaries, payer,
    moderate, biographical, constrained, national).

% Study the archival and institutional record to identify or dispute which holding event counts as 'first.' Their scholarship can destabilize or reinforce the certified threshold date depending on what documentary evidence for prior, uncertified dematerialized holdings surfaces.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, monetary_historians, observer,
    analytical, civilizational, analytical, global).

% Kept dematerialized account records (temple ledgers, merchant book-credit, clearinghouse tallies) long before the certified institutional threshold, but their holdings were never brought within regulatory or legal recognition and so do not count under this reading. They have no voice in setting the threshold and no forum in which to press a prior claim.
narrative_ontology:constraint_stakeholder(electronic_money_emergence__first_held_reading, informal_ledger_keepers, excluded,
    powerless, biographical, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(electronic_money_emergence__first_held_reading, settlement_banks).
narrative_ontology:fixing_cost_class(electronic_money_emergence__first_held_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Certifying a discrete institutional moment as the origin of digital money coordinates legal, regulatory, and statistical systems around a single recognized threshold — enabling consistent reserve accounting, deposit insurance boundaries, and monetary aggregate classification (M1 vs M4/M5 lines) built on a shared origin date.
% TRANSFER_FUNCTION: Moves legitimacy and the associated legal/regulatory privileges (credit expansion rights, statistical recognition, insured-deposit status) toward the certified first institutional holder and the authorities who certify it, while moving comparable but uncertified holdings (cash practice, shadow ledgers, informal book-credit) outside the newly bounded category of 'money' — with material consequences for who gets formal financial inclusion.
% ABSENT_VOICES: Informal ledger-keepers and prior uncertified holders of dematerialized value (temple/merchant/clearinghouse book-credit systems) would object that the threshold is arbitrary and privileges institutional recognition over functional equivalence; they are not party to the regulatory certification process and left no comparable documentary record that counts under this reading's evidentiary standard.
% DISAPPEARANCE_RATIONALE: If the certified 'first institutional holding' event were declared void or undated, the legal and statistical apparatus built on it (reserve treatment of ledger balances, monetary aggregate boundaries) would need a new anchor point — settlement banks and central authorities dispute this would matter little in practice (the functional system would persist), while historians and excluded informal-ledger claimants argue the anchor materially determines whose historical practice counts as 'real' money and whose does not.
% FOUNDING_PROBLEM: Regulators and monetary statisticians needed a discrete, administratively usable threshold to distinguish physical currency from dematerialized currency for legal tender law, reserve requirements, and monetary aggregate construction (M1 through M4/M5).
% FOUNDING_PROBLEM_CORROBORATION: Central bank statisticians and legal scholars outside the certifying authorities attest the classification problem remains live — every revision of monetary aggregate methodology re-litigates where the dematerialized/physical boundary sits. Monetary historians, who are not beneficiaries of the certified threshold, corroborate that the founding problem (administrative tractability of a fuzzy transition) is real but note the specific 'first holder' anchor is itself a contested administrative choice rather than a forced one.
narrative_ontology:disappearance_verdict(electronic_money_emergence__first_held_reading, contested).
narrative_ontology:founding_problem_status(electronic_money_emergence__first_held_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(electronic_money_emergence__first_held_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(electronic_money_emergence__first_held_reading, 'none', 1).
narrative_ontology:epsilon_provenance(electronic_money_emergence__first_held_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(electronic_money_emergence__first_held_reading_tests).
:- end_tests(electronic_money_emergence__first_held_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-low (0.28 at interval end) and rises slowly: this is not a high-extraction constraint by nature, but the coordination benefit of a shared administrative threshold is bundled with a quiet asymmetry — the certifying institutions' own holdings become the reference point for legal recognition, and later revisions of what counts as 'money' for statistical purposes track back to that anchor, generating small but real distributional consequences for cash-dependent and informal actors. Suppression and theater ratio are both low and rising gently, reflecting an administrative rather than coercive mechanism: the threshold is maintained by bureaucratic continuity and legal precedent, not active policing, though enforcement (reserve requirements, deposit insurance boundaries keyed to the certified category) does require ongoing regulatory maintenance, which is why requires_active_enforcement is true.
 *
 * DIRECTIONALITY LOGIC:
 *   Settlement banks and central bank ledger authorities sit near the beneficiary end: they set and benefit from the certification, gaining legal and statistical privileges tied to the anchor event. Cash-dependent depositors and shadow intermediaries sit toward the target end: they bear the downstream costs of a category boundary they did not draw and cannot easily contest — trapped and constrained exit respectively. Informal ledger-keepers are excluded rather than coordinated: their functionally equivalent practice sits entirely outside the recognized category, which is the clearest sign of asymmetric extraction rather than neutral coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — needing an administratively tractable line between physical and dematerialized currency for legal and statistical purposes — remains live (regulators still need SOME anchor). But the specific choice of 'first institutional holder' as that anchor is not itself forced by the founding problem; alternative anchors (first technical capability, first widespread practice) were available. Classifying this as tangled_rope rather than mountain or pure snare captures that: there IS a genuine coordination function (a shared threshold enables consistent monetary statistics) AND there is asymmetric extraction (the certifying institutions' own practice becomes privileged, uncertified prior practice is excluded). Treating this as a mountain (inevitable natural fact of monetary history) would launder the arbitrary anchor-selection into apparent necessity — exactly the false-summit risk this reading must not fall into, since it is a discrete institutional/legal event, not a physical law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_anchor_arbitrariness,
    'Was the ''first institutional holder'' anchor a forced choice given the founding administrative problem, or one arbitrary selection among several equally workable anchors (first technical capability, first widespread adoption, first statistical measurement)?',
    'Comparative institutional history: examine whether jurisdictions that chose different anchors (e.g., first regulatory definition vs. first documented holding) produced materially different monetary aggregate boundaries or legal consequences.',
    'If the anchor choice is arbitrary rather than forced, the beneficiary asymmetry (certifying institutions'' own practice being privileged) is closer to pure extraction riding on a real coordination need; if forced by administrative necessity, more of the measured extraction is properly coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_anchor_arbitrariness, conceptual, 'Whether the specific institutional-holding anchor was necessary or one contestable choice among alternatives.').

omega_variable(
    sibling_reading_priority_dispute,
    'Do the sibling readings (became_thinkable_reading, m4_m5_collapse_reading) describe genuinely earlier or more fundamental emergence events, such that this reading''s certified threshold is itself downstream of a prior, uncertified emergence?',
    'Cross-reading archival synthesis: locate documented instances of dematerialized-currency practice or conceptual articulation prior to the certified institutional holding date, and assess whether they satisfy the same functional criteria this reading requires (distinguishability from physical notes) without institutional certification.',
    'If such instances exist and are functionally equivalent, this reading''s emergence date is a certification artifact rather than a true origin point — strengthening the case that the beneficiary asymmetry is extraction dressed as discovery, not discovery itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_priority_dispute, conceptual, 'Whether this reading''s institutional threshold is prior to or downstream of the sibling readings'' emergence points.').

omega_variable(
    informal_practice_documentary_gap,
    'Is the absence of informal ledger-keepers'' claims from the historical record due to genuine absence of functionally equivalent prior practice, or due to a documentary/archival bias that simply never recorded or preserved evidence of such practice?',
    'Archival and anthropological research into pre-certification bookkeeping, clearinghouse, and temple/merchant credit systems across multiple regions, assessed against the same distinguishability criterion this reading applies to the certified holder.',
    'If a documentary bias is found, the exclusion of informal_ledger_keepers is more clearly an artifact of whose records get taken seriously by certifying authorities, sharpening the victim/excluded-voice reading of this constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(informal_practice_documentary_gap, empirical, 'Whether informal ledger-keepers'' absence from the record reflects genuine absence of practice or an archival/documentary bias.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(electronic_money_emergence__first_held_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(elec_tr_t0, electronic_money_emergence__first_held_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(elec_tr_t12, electronic_money_emergence__first_held_reading, theater_ratio, 12, 0.07).
narrative_ontology:measurement(elec_tr_t24, electronic_money_emergence__first_held_reading, theater_ratio, 24, 0.09).
narrative_ontology:measurement(elec_tr_t36, electronic_money_emergence__first_held_reading, theater_ratio, 36, 0.11).
narrative_ontology:measurement(elec_tr_t48, electronic_money_emergence__first_held_reading, theater_ratio, 48, 0.13).
narrative_ontology:measurement(elec_tr_t60, electronic_money_emergence__first_held_reading, theater_ratio, 60, 0.15).

% Extraction over time
narrative_ontology:measurement(elec_be_t0, electronic_money_emergence__first_held_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(elec_be_t12, electronic_money_emergence__first_held_reading, base_extractiveness, 12, 0.16).
narrative_ontology:measurement(elec_be_t24, electronic_money_emergence__first_held_reading, base_extractiveness, 24, 0.2).
narrative_ontology:measurement(elec_be_t36, electronic_money_emergence__first_held_reading, base_extractiveness, 36, 0.23).
narrative_ontology:measurement(elec_be_t48, electronic_money_emergence__first_held_reading, base_extractiveness, 48, 0.26).
narrative_ontology:measurement(elec_be_t60, electronic_money_emergence__first_held_reading, base_extractiveness, 60, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(elec_su_t0, electronic_money_emergence__first_held_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(elec_su_t12, electronic_money_emergence__first_held_reading, suppression_requirement, 12, 0.13).
narrative_ontology:measurement(elec_su_t24, electronic_money_emergence__first_held_reading, suppression_requirement, 24, 0.16).
narrative_ontology:measurement(elec_su_t36, electronic_money_emergence__first_held_reading, suppression_requirement, 36, 0.18).
narrative_ontology:measurement(elec_su_t48, electronic_money_emergence__first_held_reading, suppression_requirement, 48, 0.2).
narrative_ontology:measurement(elec_su_t60, electronic_money_emergence__first_held_reading, suppression_requirement, 60, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(electronic_money_emergence__first_held_reading, information_standard).
narrative_ontology:boltzmann_floor_override(electronic_money_emergence__first_held_reading, 0.05).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, electronic_money_emergence__became_thinkable_reading).
narrative_ontology:affects_constraint(electronic_money_emergence__first_held_reading, electronic_money_emergence__m4_m5_collapse_reading).

% DUAL FORMULATION NOTE:
% This is one of three constraint stories decomposing the natural-language claim 'digital money emerged.' The became_thinkable_reading locates emergence in conceptual/technical possibility, prior to any institutional measurement, and carries near-zero extraction (no institution yet exists to certify or exclude anyone). The m4_m5_collapse_reading denies a discrete emergence event altogether, treating 'electronic money' as a category retroactively constructed by statistical aggregate methodology — its ε attaches to the epistemic authority of monetary statisticians, not to any institutional beneficiary. This file (first_held_reading) is the only one of the three with a genuine beneficiary/victim structure, because it is the only reading that ties emergence to a certifiable institutional event with legal and regulatory consequences for who counts as holding 'real' money.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
