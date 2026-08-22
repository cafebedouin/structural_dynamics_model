% ============================================================================
% CONSTRAINT STORY: constitutional_secularism__principled_intervention_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_secularism__principled_intervention_reading, []).

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
 *   constraint_id: constitutional_secularism__principled_intervention_reading
 *   human_readable: State Intervention in Religious Affairs for Social Reform (Principled Intervention Reading)
 *   domain: constitutional_law/political_theory/religious_governance
 *
 * SUMMARY:
 *   A constitutional order permits the state to enter religious affairs —
 *   opening institutions to excluded groups, administering endowments,
 *   reforming personal law — where social reform and the protection of weaker
 *   sections within communities justify it. This story instantiates ONE
 *   reading of the constitutional_secularism kernel: the
 *   principled_intervention_reading, under which intervention is legitimate
 *   but bounded by a retained sphere of religious autonomy. The sibling
 *   readings (strict_neutrality_reading, reformist_reading) are separate
 *   constraints with their own epsilon values and are NOT averaged into this
 *   file. The claim/metric gap is deliberate: the reading CLAIMS tangled_rope
 *   (genuine protective coordination entangled with asymmetric state reach),
 *   while the metrics are authored independently from the doctrine's observed
 *   operation — rising extraction accumulation, maturing enforcement
 *   machinery, and growing performative compliance. The engine measures the
 *   divergence; nothing here reconciles claim to metrics.
 *
 * KEY AGENTS:
 *   - reform_legislatures: agenda-setter (institutional/constrained) — enacts reform statutes, collects the regulatory jurisdiction they create
 *   - constitutional_apex_courts: agenda-setter and observer (institutional/constrained) — draws the essential-practices line that sizes the regulable surface
 *   - vulnerable_community_members: primary intended beneficiary (powerless/trapped) — receives enforceable access and protection
 *   - majority_temple_establishments: primary institutional payer (organized/identity_locked) — absorbs administration and compliance burdens
 *   - minority_denominations: payer (moderate/constrained) — bears disproportionate exposure to intervention proposals
 *   - traditional_practitioners: payer (powerless/identity_locked) — vocations bound to regulated practices
 *   - secular_reform_movements: beneficiary (organized/mobile) — supplies the litigation that operationalizes the power
 *   - unconsulted_community_members: excluded (powerless/trapped) — regulated without representation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_secularism__principled_intervention_reading, 0.55).
domain_priors:suppression_score(constitutional_secularism__principled_intervention_reading, 0.6).
domain_priors:theater_ratio(constitutional_secularism__principled_intervention_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(constitutional_secularism__principled_intervention_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_secularism__principled_intervention_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_secularism__principled_intervention_reading, "State Intervention in Religious Affairs for Social Reform (Principled Intervention Reading)").
narrative_ontology:topic_domain(constitutional_secularism__principled_intervention_reading, "constitutional_law/political_theory/religious_governance").

domain_priors:requires_active_enforcement(constitutional_secularism__principled_intervention_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_secularism__principled_intervention_reading, '79d7f16b-8944-4ebe-9931-913d2b205976').
narrative_ontology:cs_kernel_codification('79d7f16b-8944-4ebe-9931-913d2b205976', fixed_text).
narrative_ontology:cs_authority_grounding('79d7f16b-8944-4ebe-9931-913d2b205976', lineage).
narrative_ontology:cs_interpretation_layer_present('79d7f16b-8944-4ebe-9931-913d2b205976').
narrative_ontology:cs_reading_relation('79d7f16b-8944-4ebe-9931-913d2b205976', constitutional_secularism__strict_neutrality_reading, forecloses).
narrative_ontology:cs_reading_relation('79d7f16b-8944-4ebe-9931-913d2b205976', constitutional_secularism__reformist_reading, coexists_with).
narrative_ontology:cs_axiom('79d7f16b-8944-4ebe-9931-913d2b205976', foundational, reform_objectives_justify_differential_treatment).
narrative_ontology:cs_axiom_status(reform_objectives_justify_differential_treatment, holdable).
narrative_ontology:cs_axiom_grounding('79d7f16b-8944-4ebe-9931-913d2b205976', reform_objectives_justify_differential_treatment, instrumental).
narrative_ontology:cs_axiom('79d7f16b-8944-4ebe-9931-913d2b205976', foundational, religious_autonomy_presumptively_retained).
narrative_ontology:cs_axiom_status(religious_autonomy_presumptively_retained, holdable).
narrative_ontology:cs_axiom_grounding('79d7f16b-8944-4ebe-9931-913d2b205976', religious_autonomy_presumptively_retained, deontological).
narrative_ontology:cs_axiom('79d7f16b-8944-4ebe-9931-913d2b205976', secondary, weaker_section_protection_is_compelling_state_interest).
narrative_ontology:cs_axiom_status(weaker_section_protection_is_compelling_state_interest, holdable).
narrative_ontology:cs_axiom_grounding('79d7f16b-8944-4ebe-9931-913d2b205976', weaker_section_protection_is_compelling_state_interest, deontological).
narrative_ontology:cs_reference_frame('79d7f16b-8944-4ebe-9931-913d2b205976', transformative_secular_settlement).
narrative_ontology:cs_drift_state('79d7f16b-8944-4ebe-9931-913d2b205976', contemporary_majoritarian_politics, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('79d7f16b-8944-4ebe-9931-913d2b205976', '').
narrative_ontology:cs_kernel_id(constitutional_secularism__principled_intervention_reading, constitutional_secularism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, vulnerable_community_members).
narrative_ontology:constraint_beneficiary(constitutional_secularism__principled_intervention_reading, secular_reform_movements).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, majority_temple_establishments).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, minority_denominations).
narrative_ontology:constraint_victim(constitutional_secularism__principled_intervention_reading, traditional_practitioners).
narrative_ontology:constraint_vindicates(constitutional_secularism__principled_intervention_reading, transformative_constitutionalism).
narrative_ontology:constraint_vindicates(constitutional_secularism__principled_intervention_reading, essential_practices_jurisprudence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts statutes opening religious institutions to excluded groups, placing endowments under public administration, and reforming personal law. Campaigns on its reform record and collects the regulatory jurisdiction each statute creates. Bound by the constitutional text and by judicial review; cannot quietly retreat from enacted reform without visible political cost.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, reform_legislatures, agenda_setter,
    institutional, generational, constrained, national).

% Decides case by case which practices count as essential and immune and which are regulable adjuncts, thereby drawing and redrawing the outer edge of the intervention power. Its docket grows with every community dispute the doctrine channels toward it. Bound by precedent and by the founding text it interprets.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, constitutional_apex_courts, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(constitutional_secularism__principled_intervention_reading, constitutional_apex_courts, observer).

% Members excluded from worship, religious education, or equal personal-law treatment inside their own communities. Gain legally enforceable access and protection through the intervention power. Cannot leave their communities without losing family, livelihood, and belonging. Rarely appear as parties in the proceedings decided in their name.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, vulnerable_community_members, beneficiary,
    powerless, biographical, trapped, national).

% Large endowed institutions whose administration, finances, staffing, and ritual calendars fall under state departments and reform statutes. Litigate autonomy claims continuously and absorb compliance costs. Their religious function is inseparable from the practices being regulated, so exit would mean ceasing to be what they are.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, majority_temple_establishments, payer,
    organized, generational, identity_locked, national).

% Smaller faith communities whose schools, personal law, and dress or dietary practices face recurring intervention proposals. Hold fewer endowments and less litigation capacity than the large majority establishments. Fear the reform rationale reaches their practices faster than it reaches majority ones. Exit means emigration or dissolution.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, minority_denominations, payer,
    moderate, generational, constrained, national).

% Priests, ritual specialists, and customary office-holders whose vocations depend on practices now subject to licensing, rostering, or prohibition. Comply under protest and bear the daily friction of administration. Their calling is the practice itself, so leaving means abandoning their life's work.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, traditional_practitioners, payer,
    powerless, biographical, identity_locked, regional).

% Civil-society organizations campaigning for temple entry, caste equality, and personal-law reform. Use the intervention power as their principal legal instrument and supply much of the litigation that defines its scope. Can redirect their energies to other campaigns if this instrument closes.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, secular_reform_movements, beneficiary,
    organized, biographical, mobile, national).

% Community members whose practices are litigated between state counsel and traditional leadership. Their own views on their tradition are neither solicited nor represented in the proceedings that regulate them, and they learn the outcomes from public reporting after the fact.
narrative_ontology:constraint_stakeholder(constitutional_secularism__principled_intervention_reading, unconsulted_community_members, excluded,
    powerless, biographical, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform constitutional floor of access and equal treatment enforceable against private religious gatekeeping, solving the problem that intra-community hierarchies persist indefinitely when remedy depends solely on the consent of the gatekeepers who benefit from them.
% TRANSFER_FUNCTION: Moves regulatory authority over religious practice and institution administration from community gatekeepers to state legislatures and courts; moves enforceable access and protection rights to weaker-section members; moves day-to-day discretion over tradition from practitioners to public officials.
% ABSENT_VOICES: The people the doctrine protects are mostly absent from the rooms where its scope is set: reform cases are argued between state lawyers and traditional leadership, with intended beneficiaries rarely appearing as parties. Minority denominations without litigation budgets are similarly under-heard, and ordinary practitioners learn of new obligations after they take effect.
% DISAPPEARANCE_RATIONALE: Overnight repeal would strip the constitutional foundation from decades of temple-entry, endowment-administration, and personal-law statutes; access gains would revert to privately contestable privileges; religious institutions would reclaim administration and membership rules; and the courts would lose the docket that currently structures the entire relationship between state authority and religious governance.
% FOUNDING_PROBLEM: The framers confronted intra-religious hierarchies — exclusion from worship, caste disability, unequal personal law — entrenched deeply enough that internal reform had failed for generations. The intervention power was built so the new republic could complete a program of social reform that religious gatekeepers had blocked.
% FOUNDING_PROBLEM_CORROBORATION: Constituent assembly debates and mid-century reform commission records, held outside any benefiting party, attest the founding problem. Contemporary civil-liberties litigation and social surveys attest that exclusionary practices persist, supporting the live-problem reading. Minority-rights organizations and religious-autonomy scholars, also outside the beneficiary set, attest that the doctrine's application has drifted toward selective targeting, supporting the shifted-function reading. Both readings carry extra-beneficiary attestation; neither is self-asserted.
narrative_ontology:disappearance_verdict(constitutional_secularism__principled_intervention_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_secularism__principled_intervention_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_secularism__principled_intervention_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_secularism__principled_intervention_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_secularism__principled_intervention_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_secularism__principled_intervention_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_secularism__principled_intervention_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_secularism__principled_intervention_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits mid-range (0.55 at interval end) because the doctrine delivers real, documented protections — temple entry, personal-law equalization — while simultaneously accumulating regulatory jurisdiction and applying the reform rationale unevenly across communities. Suppression (0.60) is authored as a raw structural property, unscaled by power or scope: participation in the religious order is compulsory for members and institutions alike, and the enforcement bureaucracy that administers endowments and rituals has matured steadily over the interval (rising suppression_requirement series models enforcement hardening, not merely shifting extraction). Theater_ratio (0.30) reflects a growing share of performative activity — reform statutes passed with weak enforcement machinery, oversight committees that report without acting — though the core protective function remains substantively performed. Accessibility_collapse (0.45) is moderate: internal reform, civil-society pressure, and litigation remain partially available alternatives, but they atrophy once the state channel exists. Resistance (0.65) is high and continuous: autonomy litigation, institutional non-compliance, and political backlash are permanent features. All three temporal series share one grid (t=0..72 step 12) so no metric is sampled against another's end-state. Receipt surface: the gains — regulatory jurisdiction, administrative control, and the political credit of reform — demonstrably accrue to the legislative seat, with the court seat sharing secondarily through docket and doctrinal authority, hence gain_flow names reform_legislatures. Fixing cost is prohibitive: any actor able to remove the power (a constituent process or the apex court) would forfeit settled, relied-upon protections whose loss outweighs the burden removed, so removal is not a live option even for critics.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structural data. From the agenda-setter seats the arrangement is a legitimate reform instrument they administer and are electorally and institutionally rewarded for maintaining. From the payer seats — particularly the identity-locked establishments and practitioners — the same structure operates as compulsory subordination of religious life to state management, with exit effectively unavailable. From the beneficiary seats it computes as protection, provided the agency deficit documented in the omegas does not invert their experience. The engine derives these per-seat classifications from power, exit, and directional position; this commentary predicts the divergence without adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (vulnerable_community_members, secular_reform_movements) derive low directionality — the structure subsidizes them — though the first seat's subsidy is qualified by its exclusion from the proceedings conducted in its name (tracked by the protected_class_agency_deficit omega rather than by an override, since the derivation is broadly right and the qualification is a matter of degree). Declared victims (majority_temple_establishments, minority_denominations, traditional_practitioners) derive high directionality, amplified for the identity-locked seats whose exit would require abandoning the very practice regulated. The two state seats derive from their enforcement position: they neither pay the burdens nor simply collect them — they administer the structure, sitting nearer the beneficiary end through jurisdiction gained. No directionality_overrides are needed: the beneficiary/victim declarations plus exit profiles already produce the correct relative ordering, and the coarse power-atom keying of overrides would misapply corrections across the multiple powerless seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — intra-religious hierarchy that internal reform could not dislodge — is contested rather than dead: exclusionary practices persist (supporting liveness) while the doctrine's application has drifted toward selective targeting (supporting obsolescence of the original justification in part of its operating range). Because founding_problem_status is contested and disappearance_verdict is world_rearranges, the mismatch consumer should not fire the dead-mandate/zombie flag; the arrangement still rearranges the world if removed. The classification prevents two opposite mislabels: reading the doctrine as pure coordination ignores the accumulated, asymmetrically applied regulatory burden (the tangled half); reading it as pure extraction erases the documented access gains that give the coordination function reality (the rope half). The rising base_extractiveness series is the accumulation signal worth monitoring: if cross-community asymmetry is confirmed (majoritarian_capture_asymmetry omega), the profile degrades toward the extractive pole for the burdened communities specifically.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the constitutional_secularism kernel; how would the sibling readings restructure the beneficiary/victim surface and the classification?',
    'Instantiate strict_neutrality_reading and reformist_reading as separate stories and compare victim sets, epsilon, and enforcement requirements across the family; the disagreement lives at the permission/duty axis and at the retained-autonomy limit.',
    'Under strict_neutrality_reading the intervention power itself becomes the violation — epsilon rises sharply and the victim set broadens to every regulated institution. Under reformist_reading the victim set narrows to oppressive practices alone and epsilon falls. The tangled-rope profile authored here holds only for the principled-intervention middle; it is not a verdict on the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer-frame uncertainty: which reading of the secularism kernel governs, and what each sibling would change structurally.').

omega_variable(
    majoritarian_capture_asymmetry,
    'Is the intervention power applied proportionally across communities, or does it systematically burden minority practices while the largest institutions retain practical autonomy — or the reverse?',
    'Cross-community audit of intervention statutes and judgments over the full interval, normalized by community population share and institutional endowment size.',
    'Confirmed systematic asymmetry degrades the profile toward the extractive pole for the burdened community specifically and validates the capture half of this reading''s expected structural delta; proportional application strengthens the coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majoritarian_capture_asymmetry, empirical, 'Whether the reform rationale masks selective targeting across communities.').

omega_variable(
    protected_class_agency_deficit,
    'Do members of the protected weaker sections experience the intervention as protection they sought, or as external management of their communities carried out in their name?',
    'Participation and attitude studies of affected members: rates of self-initiated versus state-initiated reform litigation, and post-intervention assessment by the people the access gains were meant for.',
    'If experienced as management, the beneficiary declaration overstates the subsidy, the coordination-function claim weakens, and the profile drifts toward extraction despite the formal beneficiary structure; if sought and welcomed, the coordination half firms up.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protected_class_agency_deficit, empirical, 'Whether the declared beneficiaries experience subsidy or control.').

omega_variable(
    essentiality_boundary_stability,
    'Where does the court-drawn line between immune essential practice and regulable secular adjunct sit, and is it stable across the interval?',
    'Doctrinal coding of essential-practice holdings across the interval, tracking whether the immune core is shrinking, stable, or growing.',
    'A shrinking essential core widens the regulable surface and raises the burden on every practitioner and establishment seat; a stable or growing core bounds the intervention power and stabilizes the current profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(essentiality_boundary_stability, conceptual, 'Instability of the doctrine''s internal limit on state reach.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_secularism__principled_intervention_reading, 0, 72).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pir_tr_t0, constitutional_secularism__principled_intervention_reading, theater_ratio, 0, 0.14).
narrative_ontology:measurement_basis(pir_tr_t0, observed).
narrative_ontology:measurement(pir_tr_t12, constitutional_secularism__principled_intervention_reading, theater_ratio, 12, 0.17).
narrative_ontology:measurement_basis(pir_tr_t12, observed).
narrative_ontology:measurement(pir_tr_t24, constitutional_secularism__principled_intervention_reading, theater_ratio, 24, 0.2).
narrative_ontology:measurement_basis(pir_tr_t24, observed).
narrative_ontology:measurement(pir_tr_t36, constitutional_secularism__principled_intervention_reading, theater_ratio, 36, 0.23).
narrative_ontology:measurement_basis(pir_tr_t36, observed).
narrative_ontology:measurement(pir_tr_t48, constitutional_secularism__principled_intervention_reading, theater_ratio, 48, 0.26).
narrative_ontology:measurement_basis(pir_tr_t48, observed).
narrative_ontology:measurement(pir_tr_t60, constitutional_secularism__principled_intervention_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement_basis(pir_tr_t60, observed).
narrative_ontology:measurement(pir_tr_t72, constitutional_secularism__principled_intervention_reading, theater_ratio, 72, 0.3).
narrative_ontology:measurement_basis(pir_tr_t72, observed).

% Extraction over time
narrative_ontology:measurement(pir_be_t0, constitutional_secularism__principled_intervention_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(pir_be_t0, observed).
narrative_ontology:measurement(pir_be_t12, constitutional_secularism__principled_intervention_reading, base_extractiveness, 12, 0.42).
narrative_ontology:measurement_basis(pir_be_t12, observed).
narrative_ontology:measurement(pir_be_t24, constitutional_secularism__principled_intervention_reading, base_extractiveness, 24, 0.46).
narrative_ontology:measurement_basis(pir_be_t24, observed).
narrative_ontology:measurement(pir_be_t36, constitutional_secularism__principled_intervention_reading, base_extractiveness, 36, 0.49).
narrative_ontology:measurement_basis(pir_be_t36, observed).
narrative_ontology:measurement(pir_be_t48, constitutional_secularism__principled_intervention_reading, base_extractiveness, 48, 0.52).
narrative_ontology:measurement_basis(pir_be_t48, observed).
narrative_ontology:measurement(pir_be_t60, constitutional_secularism__principled_intervention_reading, base_extractiveness, 60, 0.54).
narrative_ontology:measurement_basis(pir_be_t60, observed).
narrative_ontology:measurement(pir_be_t72, constitutional_secularism__principled_intervention_reading, base_extractiveness, 72, 0.55).
narrative_ontology:measurement_basis(pir_be_t72, observed).

% Suppression requirement over time
narrative_ontology:measurement(pir_su_t0, constitutional_secularism__principled_intervention_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(pir_su_t0, observed).
narrative_ontology:measurement(pir_su_t12, constitutional_secularism__principled_intervention_reading, suppression_requirement, 12, 0.48).
narrative_ontology:measurement_basis(pir_su_t12, observed).
narrative_ontology:measurement(pir_su_t24, constitutional_secularism__principled_intervention_reading, suppression_requirement, 24, 0.51).
narrative_ontology:measurement_basis(pir_su_t24, observed).
narrative_ontology:measurement(pir_su_t36, constitutional_secularism__principled_intervention_reading, suppression_requirement, 36, 0.54).
narrative_ontology:measurement_basis(pir_su_t36, observed).
narrative_ontology:measurement(pir_su_t48, constitutional_secularism__principled_intervention_reading, suppression_requirement, 48, 0.57).
narrative_ontology:measurement_basis(pir_su_t48, observed).
narrative_ontology:measurement(pir_su_t60, constitutional_secularism__principled_intervention_reading, suppression_requirement, 60, 0.59).
narrative_ontology:measurement_basis(pir_su_t60, observed).
narrative_ontology:measurement(pir_su_t72, constitutional_secularism__principled_intervention_reading, suppression_requirement, 72, 0.6).
narrative_ontology:measurement_basis(pir_su_t72, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_secularism__principled_intervention_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, constitutional_secularism__strict_neutrality_reading).
narrative_ontology:affects_constraint(constitutional_secularism__principled_intervention_reading, constitutional_secularism__reformist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'constitutional secularism' decomposes into three structurally distinct readings with distinct epsilon values, per the epsilon-invariance principle. strict_neutrality_reading is the baseline (interference itself is the deviation); this principled_intervention_reading licenses bounded interference and thereby creates downstream legitimacy conditions for reformist_reading (once reform-motivated entry is legitimate, the stronger duty-claim gains argumentative footing), while logically foreclosing strict neutrality within any single framework. Each member carries its own beneficiaries, victims, and metrics; none averages over the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
