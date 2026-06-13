% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__exogenous_override_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: marriage_commitment_reversal__exogenous_override_reading
 *   human_readable: Federal Coercion of Marriage Practice Reversal (Exogenous Override Reading)
 *   domain: religious/political/institutional
 *
 * SUMMARY:
 *   Between 1880 and 1910, the federal government enforces monogamy on the
 *   LDS church through territorial incorporation denial, property seizure,
 *   and criminal prosecution. The church formally suspends polygamous
 *   marriage practice in the 1890 Manifesto while preserving Section 132 (the
 *   revelation authorizing plural marriage) in canon doctrine. This
 *   constraint embodies EXOGENOUS OVERRIDE: the practice reversal is
 *   extracted by federal coercion, not internal doctrinal reinterpretation.
 *   Section 132 remains doctrine; the institutional act is one of public
 *   compliance under duress, not of renouncing the principle. The
 *   doctrine-practice gap persists because the constraint is external
 *   enforcement, not internal doctrine revision.
 *
 * KEY AGENTS:
 *   - LDS institutional leadership: agenda-setter under duress, administers compliance; trapped between institutional survival and doctrinal coherence
 *   - Faithful plural practitioners: identity-locked payers, face excommunication for practicing doctrine
 *   - Federal government: external agenda-setter, enforces monogamy indifferent to theology
 *   - Polygamous families: trapped payers, lose legal marital status
 *   - Utah territory: constrained payer, political incorporation held hostage to compliance
 *   - Observant dissidents: excluded objectors, believe Section 132 still binding
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, 0.78).
domain_priors:suppression_score(marriage_commitment_reversal__exogenous_override_reading, 0.81).
domain_priors:theater_ratio(marriage_commitment_reversal__exogenous_override_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 0.62).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__exogenous_override_reading, snare).
narrative_ontology:human_readable(marriage_commitment_reversal__exogenous_override_reading, "Federal Coercion of Marriage Practice Reversal (Exogenous Override Reading)").
narrative_ontology:topic_domain(marriage_commitment_reversal__exogenous_override_reading, "religious/political/institutional").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__exogenous_override_reading, '140a2548-cb01-4381-911a-1b421b7ca7f0').
narrative_ontology:cs_kernel_codification('140a2548-cb01-4381-911a-1b421b7ca7f0', fixed_text).
narrative_ontology:cs_authority_grounding('140a2548-cb01-4381-911a-1b421b7ca7f0', extraction).
narrative_ontology:cs_interpretation_layer_present('140a2548-cb01-4381-911a-1b421b7ca7f0').
narrative_ontology:cs_reading_relation('140a2548-cb01-4381-911a-1b421b7ca7f0', marriage_commitment_reversal__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('140a2548-cb01-4381-911a-1b421b7ca7f0', marriage_commitment_reversal__practice_doctrine_gap, coexists_with).
narrative_ontology:cs_axiom('140a2548-cb01-4381-911a-1b421b7ca7f0', foundational, practice_reversal_coercive_extraction).
narrative_ontology:cs_axiom_status(practice_reversal_coercive_extraction, holdable).
narrative_ontology:cs_axiom_grounding('140a2548-cb01-4381-911a-1b421b7ca7f0', practice_reversal_coercive_extraction, empirically_contingent).
narrative_ontology:cs_axiom('140a2548-cb01-4381-911a-1b421b7ca7f0', secondary, doctrine_preserved_under_duress).
narrative_ontology:cs_axiom_status(doctrine_preserved_under_duress, holdable).
narrative_ontology:cs_axiom_grounding('140a2548-cb01-4381-911a-1b421b7ca7f0', doctrine_preserved_under_duress, instrumental).
narrative_ontology:cs_reference_frame('140a2548-cb01-4381-911a-1b421b7ca7f0', section_132_binding_commitment).
narrative_ontology:cs_drift_state('140a2548-cb01-4381-911a-1b421b7ca7f0', federal_enforcement_aftermath, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('140a2548-cb01-4381-911a-1b421b7ca7f0', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, federal_territorial_control).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_institutional_sovereignty).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_faithful_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, monogamous_settlers).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, frontier_anti_polygamy_activists).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_institutional_leadership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, polygamous_families).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, utah_territory).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__exogenous_override_reading, federal_supremacy_doctrine).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__exogenous_override_reading, state_polygamy_prohibition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Under federal threat of territorial disincorporation, property seizure, and criminal prosecution of members, formally suspends polygamous marriage practice in the 1890 Manifesto. Preserves Section 132 doctrine in canon but enforces monogamy-only practice through internal discipline and excommunication of continuing polygamists. Trapped between institutional survival and doctrinal coherence; administers the constraint through institutional machinery that performs both compliance and theological preservation simultaneously.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, lds_institutional_leadership, agenda_setter,
    institutional, civilizational, trapped, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__exogenous_override_reading, lds_institutional_leadership, payer).

% Believe plural marriage is divinely sanctioned by Section 132; after 1890 face excommunication if they continue the practice. Exit requires abandoning both the doctrinal understanding and the relational bonds of their plural families. Compliance is coerced through institutional expulsion coupled with legal dissolution of plural marriages and criminal prosecution. Identity is fused with the commitment to plural marriage as doctrine.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, lds_faithful_practitioners, payer,
    powerless, biographical, identity_locked, national).

% Uses territorial incorporation denial, property seizure (Reynolds v. United States 1878 upholds congressional authority to suppress polygamy in territories), and criminal prosecution as leverage. Does not negotiate theology; enforces monogamy as the sole legal marriage regime across expanding federal territory. Indifferent to whether the church renounces the theological principle—compliance with law is the only requirement.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, federal_government, agenda_setter,
    institutional, generational, analytical, national).

% Plural marriages become illegal after 1890; women and children lose marital legal status and inheritance rights; property claims become legally unenforceable. Those who continue the practice face federal criminal prosecution. Trapped between commitment to their relational structure and federal law; many families dissolve or are driven into hiding.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, polygamous_families, payer,
    powerless, biographical, trapped, regional).

% Denied statehood until polygamy is publicly renounced and the LDS church shows compliance with federal marriage law. The constraint ties political incorporation to institutional discipline; Utah cannot achieve statehood while plural marriage appears as institutional practice. Statehood comes in 1896, conditional on demonstrated compliance and federal satisfaction that the church has abandoned the practice.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, utah_territory, payer,
    organized, generational, constrained, regional).

% Benefit from federal enforcement of monogamy as the sole legal marriage regime. Marriage law is standardized; LDS institutional power is constrained; federal territorial authority is vindicated. Non-LDS settlers can marry confidently within federal law; no alternative marriage regime competes for legitimacy.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, monogamous_settlers, beneficiary,
    institutional, generational, analytical, national).

% Remain convinced that Section 132 is binding revealed truth; view the 1890 reversal as institutional capitulation to federal coercion. Excluded from the institutional decision-making that produces the Manifesto; their objections are noted but overruled by leadership. Some continue the practice underground and face excommunication. Their absence from the room where compliance policy is set signals they are victims of the constraint, not parties to its negotiation.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, observant_lds_dissidents, excluded,
    moderate, biographical, identity_locked, regional).

% Campaign for federal enforcement of monogamy as a marker of civilization and fit citizenship. Benefit from the constraint's enforcement; their advocacy contributes to federal legislation (Morrill Anti-Bigamy Act 1862, Edmunds Act 1882, Edmunds-Tucker Act 1887). No direct cost to them; they collect the legitimacy of federal enforcement.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, frontier_anti_polygamy_activists, beneficiary,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__exogenous_override_reading, federal_government).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Federal law standardizes marriage as a monogamous legal category across all territories, consolidating federal jurisdiction over domestic law and preventing competing institutional marriage regimes. The constraint solves a coordination problem at the national level by eliminating legal ambiguity about marital status, property rights, and citizenship in plural marriage contexts.
% TRANSFER_FUNCTION: Moves institutional autonomy, doctrinal consistency, and relational legitimacy from the LDS church and its faithful practitioners to the federal government; in return, the church receives a pathway to territorial incorporation (conditional statehood after demonstrated compliance) and property protection (conditional on ceasing defense of plural marriage). Faithful practitioners transfer reproductive autonomy and family recognition to federal monogamy regime.
% ABSENT_VOICES: Polygamous women and their children are structurally excluded from institutional decision-making; the reversal is negotiated between male federal officials and male LDS leadership. Underground practitioners who refuse compliance have no institutional voice in the formal negotiation. Observant dissidents who believe Section 132 remains binding are overruled by institutional leadership and their objections are not heard in the room where the Manifesto is produced.
% DISAPPEARANCE_RATIONALE: If federal enforcement vanished overnight, some practitioners would resume plural marriage; the institutional discipline machinery would cease to function; underground practice would emerge publicly. Utah territory would have negotiated a different statehood path. The national marriage regime would fragment—monogamy would persist as common practice, but it would no longer be enforced by federal coercion. The LDS church would face the choice of openly renouncing Section 132 or resuming plural practice, removing the doctrine-practice gap.
% FOUNDING_PROBLEM: Federal territorial expansion and consolidated jurisdiction require unified marriage law to govern property, inheritance, spousal rights, and citizenship. Plural marriage in the Utah territory undermines federal legal authority, creates legal ambiguity about which spouse has standing in contracts and inheritance, and represents an alternative institutional marriage definition that competes with federal supremacy. Federal jurisdiction over marriage law is contested by an autonomous religious institution claiming doctrinal exemption.
% FOUNDING_PROBLEM_CORROBORATION: Federal officials and mainstream legal scholars attest the founding problem is structurally real and live through the 1880s and 1890s—congressional testimony, Supreme Court decisions (Reynolds v. United States 1878, Davis v. Beason 1890), and territorial incorporation statutes cite the jurisdictional conflict as the core issue. LDS institutional survival depends on resolving this legal conflict with federal authority. Independent historians and legal scholars not aligned with federal authority or the LDS church confirm the jurisdictional conflict is genuine: federal expansion and religious autonomy are structurally opposed on marriage law, and the constraint is the federal resolution of that conflict.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__exogenous_override_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__exogenous_override_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(marriage_commitment_reversal__exogenous_override_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint extracts institutional autonomy and doctrinal consistency without granting structural accommodation. The federal government does not negotiate theology—it enforces a secular legal regime incompatible with plural marriage. The 1886 Edmunds-Tucker Act dramatically intensifies pressure (extractiveness jumps from 0.35 to 0.58); the 1890 Manifesto formally complies but does not renounce (extractiveness rises to 0.73 by 1892 as suppression deepens through institutional excommunication). Theater ratio climbs from 0.15 to 0.62 because the constraint's persistence requires constant performance: maintaining that Section 132 is still doctrine while publicly enforcing monogamy-only practice, dissidents are disciplined, and compliance is performed for federal observers. Suppression requirement stays high (0.81) because the coercion is structural and external—only federal enforcement removal would dissolve it. Accessibility collapse (0.72) reflects that plural marriage is legally and institutionally unavailable; resistance (0.68) shows that some practitioners and dissidents continue to resist the constraint underground. The grid traces coercion asymmetrically: organizational pressure on leadership is severe (stakes_inflation 0.85 at t1); individual pressure on faithful practitioners remains high (suppression 0.82); structural pressure favors federal authority consolidation. Resistance at individual level declines as organizational suppression tightens and identity-lock deepens.
 *
 * PERSPECTIVAL GAP:
 *   From the federal seat, the constraint is a legitimate enforcement of unified marriage law against a sect claiming exemption—no doctrinal negotiation needed or offered. From the LDS institutional seat, the constraint is coercive duress that forces public compliance while doctrine remains preserved—a doctrine-practice gap the institution must manage theatrically. From the faithful practitioner seat, the constraint is an institutional capitulation to external force that invalidates their understanding of revealed truth. The engine computes directionality per seat: federal institutional (beneficiary, low d → subsidy), LDS institutional (constrained agenda-setter facing extraction, high d → high χ), faithful practitioners (powerless, identity-locked targets, very high d → maximum χ). The measured extraction is highest for those whose identity is tied to plural marriage and who lack exit; lowest for the federal government which benefits without cost.
 *
 * DIRECTIONALITY LOGIC:
 *   LDS institutional sovereignty is the victim (d near 1.0): the constraint extracts autonomy, forces performance of compliance, and creates internal discipline machinery to suppress dissent. Federal territorial control is the beneficiary (d near 0.0): it standardizes marriage law, consolidates authority, and faces no cost. Faithful practitioners and polygamous families are fully targeted (d = 1.0): identity-locked, powerless, facing institutional expulsion and legal dissolution of marriage. Utah territory is partially targeted (d ~0.65): incorporation is held hostage, but eventual statehood comes with compliance. The gap between the agenda-setter (LDS leadership, moderate d ~0.55, managing duress) and the payers (faithful practitioners, maximum d ~0.95) is diagnostic: leadership has some exit and arbitrage (survival negotiation), while practitioners have none.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resists mandatrophy mislabeling because the founding problem (federal jurisdiction over marriage law) never dies—it is SOLVED by extraction, not by coordination. The mandate is not obsolete; it is successfully enforced. A snare diagnosis is appropriate: the constraint persists because federal coercion works, not because parties benefit from coordination. The 1890 Manifesto appears as a voluntary institutional choice but is structurally a concession to federal leverage—the doctrine-practice gap is itself the sign of extraction without internal resolution. If this were a tangled_rope or scaffold, we would expect internal doctrinal evolution (reinterpretation of Section 132) or declared sunset conditions; instead, the doctrine is preserved and the practice is suppressed by institutional discipline backed by federal law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coercion_vs_revelation,
    'Is the 1890 Manifesto a response to internal divine reinterpretation (endogenous reading) or external federal coercion (exogenous reading)? The Manifesto text does not distinguish; historical evidence permits both framings.',
    'Archival analysis of LDS leadership correspondence and federal negotiation records; testimony from participants about whether the revelation was sought to resolve a pre-existing institutional commitment or emerged under federal pressure. The two readings would have different causal orderings: revelation→compliance or coercion→rhetoric-of-revelation.',
    'If exogenous coercion is established as primary, the constraint is a snare (extraction) and the theater ratio is the diagnostic feature (doctrine preserved while practice suppressed). If endogenous revelation is primary, the constraint reclassifies toward scaffold or rope (institutional reinterpretation under changed circumstances, not extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_vs_revelation, empirical, 'Whether the doctrine-practice reversal was internally motivated or externally coerced.').

omega_variable(
    doctrine_preservation_motive,
    'Why does the LDS church preserve Section 132 in canon doctrine after 1890 if the practice is suspended? Is the preservation sincere (belief that the principle remains binding, merely suspended), performative (maintaining legitimacy for potential future practice), or tactical (hedging against federal demands for complete renunciation)?',
    'Institutional history and textual analysis of how LDS theology interprets the Manifesto''s relationship to Section 132. Do contemporary sources treat Section 132 as eternally binding, temporarily suspended, or definitively abrogated? Longitudinal analysis of whether the church treats the doctrine as a live source of authority or as historical artifact.',
    'If sincere, Section 132 is a contested but preserved principle—the constraint is doctrine-practice divergence by institutional design. If performative, the preservation signals continuing vulnerability to federal pressure (the doctrine could be renounced if coercion intensifies). If tactical, the doctrine is a negotiating asset held in reserve. Each motive changes how the theater_ratio and the stability of compliance should be interpreted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(doctrine_preservation_motive, conceptual, 'The meaning of doctrinal preservation when practice is suppressed by external force.').

omega_variable(
    identity_lock_vs_constrained_exit,
    'For faithful practitioners and their families, is the exit from plural marriage identity-locked (the commitment is fused with self-concept such that exit requires identity dissolution) or is it constrained by institutional expulsion and legal sanction (external barriers that could hypothetically be removed)?',
    'Post-exit ethnographic study: what suppression persists after institutional barriers are removed? Do practitioners who leave the church continue to view plural marriage as doctrinally binding? Do their descendants who grow up outside the constraint accept monogamy as natural? Cyclical pattern analysis: do practitioners cycled through institutional discipline show reversals or permanent reorientation?',
    'If identity-locked, suppression is internalized—the constraint''s effective suppression is higher than the structural measure suggests, and removal of federal/institutional enforcement would not immediately restore plural practice. If constrained-but-not-locked, suppression is structural—removal of enforcement would permit return to practice, and some practitioners do so. This distinction affects whether the constraint should be read as extracting identity or merely extracting compliance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_constrained_exit, empirical, 'Whether suppression of plural marriage is internalized identity-lock or structural constraint.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the exogenous coercion reading (this constraint) logically foreclose the endogenous reinterpretation reading, or can both framings coexist in LDS institutional theology?',
    'LDS theological analysis: can a believer coherently hold that (1) an external federal threat caused the Manifesto to be issued AND (2) the Manifesto represents an internal divine reinterpretation? Or does one premise logically rule out the other? The answer depends on LDS doctrinal understanding of how divine revelation relates to institutional survival pressures.',
    'If the readings foreclose each other, they represent a genuine epistemic/theological split in how the LDS community understands the Manifesto—different factions hold incompatible readings. If both can coexist, the kernel contains irreducible ambiguity and the practice_doctrine_gap reading is the primary output (the Manifesto is inherently contested). Foreclosure strengthens the exogenous reading as a distinct reading; coexistence suggests the kernel itself is the constraint, not the readings of it.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical compatibility of exogenous coercion and endogenous divine reinterpretation framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__exogenous_override_reading, 1880, 1910).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1880, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1880, 0.15).
narrative_ontology:measurement_basis(marr_tr_t1880, observed).
narrative_ontology:measurement(marr_tr_t1886, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1886, 0.35).
narrative_ontology:measurement_basis(marr_tr_t1886, observed).
narrative_ontology:measurement(marr_tr_t1892, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1892, 0.52).
narrative_ontology:measurement_basis(marr_tr_t1892, observed).
narrative_ontology:measurement(marr_tr_t1898, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1898, 0.58).
narrative_ontology:measurement_basis(marr_tr_t1898, observed).
narrative_ontology:measurement(marr_tr_t1904, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1904, 0.61).
narrative_ontology:measurement_basis(marr_tr_t1904, observed).
narrative_ontology:measurement(marr_tr_t1910, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 1910, 0.62).
narrative_ontology:measurement_basis(marr_tr_t1910, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t1880, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1880, 0.35).
narrative_ontology:measurement_basis(marr_be_t1880, observed).
narrative_ontology:measurement(marr_be_t1886, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1886, 0.58).
narrative_ontology:measurement_basis(marr_be_t1886, observed).
narrative_ontology:measurement(marr_be_t1892, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1892, 0.73).
narrative_ontology:measurement_basis(marr_be_t1892, observed).
narrative_ontology:measurement(marr_be_t1898, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1898, 0.76).
narrative_ontology:measurement_basis(marr_be_t1898, observed).
narrative_ontology:measurement(marr_be_t1904, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1904, 0.78).
narrative_ontology:measurement_basis(marr_be_t1904, observed).
narrative_ontology:measurement(marr_be_t1910, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 1910, 0.78).
narrative_ontology:measurement_basis(marr_be_t1910, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1880, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1880, 0.42).
narrative_ontology:measurement_basis(marr_su_t1880, observed).
narrative_ontology:measurement(marr_su_t1886, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1886, 0.68).
narrative_ontology:measurement_basis(marr_su_t1886, observed).
narrative_ontology:measurement(marr_su_t1892, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1892, 0.79).
narrative_ontology:measurement_basis(marr_su_t1892, observed).
narrative_ontology:measurement(marr_su_t1898, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1898, 0.81).
narrative_ontology:measurement_basis(marr_su_t1898, observed).
narrative_ontology:measurement(marr_su_t1904, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1904, 0.82).
narrative_ontology:measurement_basis(marr_su_t1904, observed).
narrative_ontology:measurement(marr_su_t1910, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 1910, 0.81).
narrative_ontology:measurement_basis(marr_su_t1910, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1880, tn=1910
narrative_ontology:measurement(marr_grid_01, marriage_commitment_reversal__exogenous_override_reading, accessibility_collapse(class), 1880, 0.35).
narrative_ontology:measurement(marr_grid_02, marriage_commitment_reversal__exogenous_override_reading, accessibility_collapse(class), 1910, 0.68).
narrative_ontology:measurement(marr_grid_03, marriage_commitment_reversal__exogenous_override_reading, accessibility_collapse(individual), 1880, 0.42).
narrative_ontology:measurement(marr_grid_04, marriage_commitment_reversal__exogenous_override_reading, accessibility_collapse(individual), 1910, 0.81).
narrative_ontology:measurement(marr_grid_05, marriage_commitment_reversal__exogenous_override_reading, accessibility_collapse(organizational), 1880, 0.38).
narrative_ontology:measurement(marr_grid_06, marriage_commitment_reversal__exogenous_override_reading, accessibility_collapse(organizational), 1910, 0.72).
narrative_ontology:measurement(marr_grid_07, marriage_commitment_reversal__exogenous_override_reading, accessibility_collapse(structural), 1880, 0.32).
narrative_ontology:measurement(marr_grid_08, marriage_commitment_reversal__exogenous_override_reading, accessibility_collapse(structural), 1910, 0.65).
narrative_ontology:measurement(marr_grid_09, marriage_commitment_reversal__exogenous_override_reading, resistance(class), 1880, 0.68).
narrative_ontology:measurement(marr_grid_10, marriage_commitment_reversal__exogenous_override_reading, resistance(class), 1910, 0.35).
narrative_ontology:measurement(marr_grid_11, marriage_commitment_reversal__exogenous_override_reading, resistance(individual), 1880, 0.72).
narrative_ontology:measurement(marr_grid_12, marriage_commitment_reversal__exogenous_override_reading, resistance(individual), 1910, 0.38).
narrative_ontology:measurement(marr_grid_13, marriage_commitment_reversal__exogenous_override_reading, resistance(organizational), 1880, 0.65).
narrative_ontology:measurement(marr_grid_14, marriage_commitment_reversal__exogenous_override_reading, resistance(organizational), 1910, 0.32).
narrative_ontology:measurement(marr_grid_15, marriage_commitment_reversal__exogenous_override_reading, resistance(structural), 1880, 0.62).
narrative_ontology:measurement(marr_grid_16, marriage_commitment_reversal__exogenous_override_reading, resistance(structural), 1910, 0.28).
narrative_ontology:measurement(marr_grid_17, marriage_commitment_reversal__exogenous_override_reading, stakes_inflation(class), 1880, 0.35).
narrative_ontology:measurement(marr_grid_18, marriage_commitment_reversal__exogenous_override_reading, stakes_inflation(class), 1910, 0.72).
narrative_ontology:measurement(marr_grid_19, marriage_commitment_reversal__exogenous_override_reading, stakes_inflation(individual), 1880, 0.38).
narrative_ontology:measurement(marr_grid_20, marriage_commitment_reversal__exogenous_override_reading, stakes_inflation(individual), 1910, 0.79).
narrative_ontology:measurement(marr_grid_21, marriage_commitment_reversal__exogenous_override_reading, stakes_inflation(organizational), 1880, 0.42).
narrative_ontology:measurement(marr_grid_22, marriage_commitment_reversal__exogenous_override_reading, stakes_inflation(organizational), 1910, 0.85).
narrative_ontology:measurement(marr_grid_23, marriage_commitment_reversal__exogenous_override_reading, stakes_inflation(structural), 1880, 0.28).
narrative_ontology:measurement(marr_grid_24, marriage_commitment_reversal__exogenous_override_reading, stakes_inflation(structural), 1910, 0.61).
narrative_ontology:measurement(marr_grid_25, marriage_commitment_reversal__exogenous_override_reading, suppression(class), 1880, 0.32).
narrative_ontology:measurement(marr_grid_26, marriage_commitment_reversal__exogenous_override_reading, suppression(class), 1910, 0.75).
narrative_ontology:measurement(marr_grid_27, marriage_commitment_reversal__exogenous_override_reading, suppression(individual), 1880, 0.35).
narrative_ontology:measurement(marr_grid_28, marriage_commitment_reversal__exogenous_override_reading, suppression(individual), 1910, 0.82).
narrative_ontology:measurement(marr_grid_29, marriage_commitment_reversal__exogenous_override_reading, suppression(organizational), 1880, 0.48).
narrative_ontology:measurement(marr_grid_30, marriage_commitment_reversal__exogenous_override_reading, suppression(organizational), 1910, 0.81).
narrative_ontology:measurement(marr_grid_31, marriage_commitment_reversal__exogenous_override_reading, suppression(structural), 1880, 0.45).
narrative_ontology:measurement(marr_grid_32, marriage_commitment_reversal__exogenous_override_reading, suppression(structural), 1910, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__exogenous_override_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(marriage_commitment_reversal__exogenous_override_reading, 0.12).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal__practice_doctrine_gap).

% DUAL FORMULATION NOTE:
% The marriage_commitment_reversal kernel admits multiple structurally distinct constraint readings. This constraint (exogenous_override_reading) models the reversal as external federal coercion extracting institutional compliance while doctrine is preserved—high extractiveness, snare classification. The endogenous_reinterpretation_reading models the reversal as internal divine reinterpretation reframing God's will under changed circumstances—low extractiveness, possible scaffold or rope. The practice_doctrine_gap reading models the structural ambiguity where doctrine and practice persistently diverge. Each reading has its own ε, beneficiary/victim structure, and classification. They are linked by the same kernel (Section 132 and its institutional status) but represent different causal stories about why the practice was reversed. The exogenous reading influences both siblings by establishing federal coercion as a structural fact that any reading must accommodate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_commitment_reversal__exogenous_override_reading, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
