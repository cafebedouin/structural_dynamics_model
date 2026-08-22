% ============================================================================
% CONSTRAINT STORY: constitutional_text__judicial_supremacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_constitutional_text__judicial_supremacy_reading, []).

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
 *   constraint_id: constitutional_text__judicial_supremacy_reading
 *   human_readable: Judicial Supremacy: Final Interpretive Authority over Constitutional Text
 *   domain: constitutional theory/political philosophy/comparative law
 *
 * SUMMARY:
 *   A written constitution grants courts the last word on what it means: when
 *   legislation collides with the text, judicial invalidation settles the
 *   matter conclusively, and no ordinary political process can revise the
 *   court's determination. This story instantiates ONE reading of the
 *   constitutional_text kernel — the judicial_supremacy_reading — as a clean,
 *   epsilon-invariant constraint; the legislative-sovereignty and
 *   popular-sovereignty readings are separate files, not positions folded
 *   into this one. The arrangement coordinates genuinely (uniform
 *   authoritative meaning, predictable limits, a protected channel for rights
 *   claims) while transferring final decision power over contested questions
 *   from elected bodies to an unelected bench and the legal profession that
 *   staffs it, foreclosing rival interpretive authorities by design.
 *   Epsilon's referent is the standing judicial-supremacy arrangement itself,
 *   assessed by this reading's own lights: the reading defends the
 *   arrangement and still authors the extraction it endorses — the
 *   counter-majoritarian transfer is real even on the defender's account.
 *   Claimed type and metrics are independent authored facts: the claim states
 *   the structure I believe true; the metrics describe operation as I observe
 *   it. KEY AGENTS (by structural relationship): - constitutional_courts:
 *   Agenda-setter and primary beneficiary (institutional/identity_locked) —
 *   administers finality, collects authority and docket control -
 *   rights_claimants_minorities: Declared beneficiary (powerless/trapped) —
 *   obtains rights remedies unavailable through political channels -
 *   legal_professional_elite: Secondary beneficiary (organized/constrained) —
 *   collects interpretive-craft rents across academy, bench, and practice -
 *   democratic_electorates: Primary target (organized/trapped) — bears
 *   displacement of policy judgment and settlement of contested questions -
 *   elected_legislatures: Target (powerful/constrained) — enacts under threat
 *   of invalidation with no ordinary override -
 *   departmentalist_popular_constitutionalists: Excluded voice
 *   (moderate/mobile) — argues for rival allocations of finality, holds no
 *   institutional seat - comparative_constitutional_scholars: Analytical
 *   observer (analytical/analytical) — compares finality allocations across
 *   systems
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(constitutional_text__judicial_supremacy_reading, 0.56).
domain_priors:suppression_score(constitutional_text__judicial_supremacy_reading, 0.65).
domain_priors:theater_ratio(constitutional_text__judicial_supremacy_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, extractiveness, 0.56).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, accessibility_collapse, 0.66).
narrative_ontology:constraint_metric(constitutional_text__judicial_supremacy_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(constitutional_text__judicial_supremacy_reading, tangled_rope).
narrative_ontology:human_readable(constitutional_text__judicial_supremacy_reading, "Judicial Supremacy: Final Interpretive Authority over Constitutional Text").
narrative_ontology:topic_domain(constitutional_text__judicial_supremacy_reading, "constitutional theory/political philosophy/comparative law").

domain_priors:requires_active_enforcement(constitutional_text__judicial_supremacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(constitutional_text__judicial_supremacy_reading, 'f69e2a6b-8f0d-4b57-ab14-0ac5402662d9').
narrative_ontology:cs_kernel_codification('f69e2a6b-8f0d-4b57-ab14-0ac5402662d9', fixed_text).
narrative_ontology:cs_authority_grounding('f69e2a6b-8f0d-4b57-ab14-0ac5402662d9', lineage).
narrative_ontology:cs_interpretation_layer_present('f69e2a6b-8f0d-4b57-ab14-0ac5402662d9').
narrative_ontology:cs_reading_relation('f69e2a6b-8f0d-4b57-ab14-0ac5402662d9', constitutional_text__legislative_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('f69e2a6b-8f0d-4b57-ab14-0ac5402662d9', constitutional_text__popular_sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('f69e2a6b-8f0d-4b57-ab14-0ac5402662d9', foundational, judicial_invalidation_conclusive).
narrative_ontology:cs_axiom_status(judicial_invalidation_conclusive, holdable).
narrative_ontology:cs_axiom_grounding('f69e2a6b-8f0d-4b57-ab14-0ac5402662d9', judicial_invalidation_conclusive, conventional).
narrative_ontology:cs_axiom('f69e2a6b-8f0d-4b57-ab14-0ac5402662d9', secondary, rights_need_insulated_enforcement).
narrative_ontology:cs_axiom_status(rights_need_insulated_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('f69e2a6b-8f0d-4b57-ab14-0ac5402662d9', rights_need_insulated_enforcement, instrumental).
narrative_ontology:cs_reference_frame('f69e2a6b-8f0d-4b57-ab14-0ac5402662d9', marbury_judicial_finality_framework).
narrative_ontology:cs_drift_state('f69e2a6b-8f0d-4b57-ab14-0ac5402662d9', contemporary_popular_constitutionalism_backlash, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('f69e2a6b-8f0d-4b57-ab14-0ac5402662d9', '').
narrative_ontology:cs_kernel_id(constitutional_text__judicial_supremacy_reading, constitutional_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, rights_claimants_minorities).
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, constitutional_courts).
narrative_ontology:constraint_beneficiary(constitutional_text__judicial_supremacy_reading, legal_professional_elite).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, democratic_electorates).
narrative_ontology:constraint_victim(constitutional_text__judicial_supremacy_reading, elected_legislatures).
narrative_ontology:constraint_vindicates(constitutional_text__judicial_supremacy_reading, marbury_judicial_finality_doctrine).
narrative_ontology:constraint_vindicates(constitutional_text__judicial_supremacy_reading, insulated_rights_enforcement_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Decides which constitutional disputes reach definitive resolution, writes the operative meaning of the text through doctrine, and invalidates statutes that conflict with that meaning. Collects authority, prestige, docket control, and budget independence from the arrangement. Its members are recruited from, and return to, the legal profession that supplies its doctrine. Renouncing final interpretive authority would dissolve the institution's self-conception as guardian of the constitutional order; the bench has no institutional life outside that role.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, constitutional_courts, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(constitutional_text__judicial_supremacy_reading, constitutional_courts, beneficiary).

% Groups that cannot win protection through electoral or legislative channels bring constitutional claims and obtain remedies when courts strike down majority-enacted burdens. Their access runs through expensive litigation, standing doctrines, and doctrinal frameworks the courts themselves control; when the courts decline their claims they have no alternate forum for constitutional redress.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, rights_claimants_minorities, beneficiary,
    powerless, biographical, trapped, national).

% Supplies the judges, clerks, advocates, and academic doctrine through which constitutional meaning is produced. Collects prestige, fees, clerkship pipelines, and career rents from holding a monopoly on the interpretive craft. Members move among academy, bench, and practice, but every station depends on the same interpretive monopoly; leaving it means leaving constitutional law altogether.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, legal_professional_elite, beneficiary,
    organized, generational, constrained, national).

% Bear the cost when preferred policies are invalidated and when contested moral questions are settled by judicial doctrine rather than legislation. Their responses run through slow and uncertain appointment politics, jurisdictional proposals that face their own constitutional hurdles, and a formal amendment threshold almost never reached. They cannot opt out of the interpretive settlement; emigration is not a response to a domestic allocation of interpretive authority.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, democratic_electorates, payer,
    organized, generational, trapped, national).

% Enact statutes knowing any of them may be struck down by a body they did not elect and cannot override by ordinary means. Absorb reversal of their policy judgments, drafting-around costs, and the strategic distortion of legislating under doctrinal uncertainty. Their levers — appointments, impeachment, jurisdiction bills — are slow, collective-action-prone, and confrontational.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, elected_legislatures, payer,
    powerful, biographical, constrained, national).

% Scholars, officials, and movements arguing that constitutional interpretation legitimately belongs to each branch or ultimately to the people through amendment, convention, or electoral mobilization. They publish, litigate test cases, and lobby for override mechanisms, but hold no seat in the operative settlement: inside the courtroom the court's own precedents define the terms of argument, and their program has no institutional door.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, departmentalist_popular_constitutionalists, excluded,
    moderate, generational, mobile, national).

% Study how different systems allocate final interpretive authority — concentrated judicial review, legislative supremacy with override clauses, popular amendment and convention mechanisms — and trace the downstream effects of each allocation on rights protection, constitutional change, and inter-branch conflict. Take testimony from every seat and owe none of them deference.
narrative_ontology:constraint_stakeholder(constitutional_text__judicial_supremacy_reading, comparative_constitutional_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(constitutional_text__judicial_supremacy_reading, constitutional_courts).
narrative_ontology:fixing_cost_class(constitutional_text__judicial_supremacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles once, centrally, and uniformly the question of what the constitutional text requires whenever political branches disagree or legislation collides with it: one authoritative meaning across all jurisdictions, predictable limits on legislative action, and a protected channel for rights claims that majorities will not honor voluntarily.
% TRANSFER_FUNCTION: Moves final interpretive authority — and with it decision power over contested moral and political questions — from elected legislatures and the electorate to an unelected judiciary and the legal professionals who staff it; concretely moves outcomes as statutes are struck, policies enjoined, and legislative agendas redirected.
% ABSENT_VOICES: Departmentalists and popular constitutionalists would object that final authority belongs to each branch or to the people; they sit outside the operative conversation because the court's own precedents define admissible argument inside it, and legislative advocates of override mechanisms have no chamber in which their proposal could be enacted under this reading.
% DISAPPEARANCE_RATIONALE: If judicial finality vanished overnight, constitutional meaning would fragment across branches and levels of government, rights enforcement would migrate to political channels with wildly uneven results, legislatures would reclaim interpretive authority or fight over it, and the legal profession's structure — schools, clerkships, appellate practice — would reorganize around whatever successor settlement emerged.
% FOUNDING_PROBLEM: How to give a written constitution binding force against the political branches that operate under it: securing rights and limiting government through an institution insulated from electoral pressure, and ending inter-branch standoffs over what the text permits.
% FOUNDING_PROBLEM_CORROBORATION: Historians document recurring majoritarian rights violations — sedition prosecutions, internments, disenfranchisement regimes — that political channels failed to correct; comparative constitutional design literature records the persistent limit-enforcement problem across systems; even legislative-supremacy advocates concede the problem exists while disputing the judicial solution. Attestation comes from outside the benefiting parties: the historical record and cross-national scholarship, not the courts or the bar.
narrative_ontology:disappearance_verdict(constitutional_text__judicial_supremacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(constitutional_text__judicial_supremacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(constitutional_text__judicial_supremacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth+rescue1', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(constitutional_text__judicial_supremacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(constitutional_text__judicial_supremacy_reading, 0.56, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(constitutional_text__judicial_supremacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(constitutional_text__judicial_supremacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(constitutional_text__judicial_supremacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness 0.56: the arrangement transfers real decision power (statutes die, agendas bend) but delivers services in exchange — uniform meaning, rights enforcement, credible commitment — so extraction is substantial without being predatory. Suppression 0.65 is a raw structural property, unscaled by power or scope: override, departmentalism, and popular interpretation are foreclosed BY DESIGN — that foreclosure is the reading's defining feature — while amendment remains formally open but practically remote, keeping suppression below totality. Theater_ratio 0.28: adjudication is overwhelmingly functional, but a growing share of institutional activity is legitimacy maintenance (ceremonial neutrality, opinion rhetoric, public defense of the bench) rather than dispute resolution. Accessibility_collapse 0.66: once finality is understood, rival allocations collapse as practical options though they survive intellectually. Resistance 0.6: court-curbing proposals, appointment wars, jurisdiction bills, and scholarly assault are persistent and organized. The temporal series run on one shared grid (t=0..60, step 10) with all three metrics authored at every point; extraction and enforcement-hardening rise together as judicial power expanded from rights protection into culture-war settlement, with theater rising as legitimacy defense grew relative to adjudication.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seat should compute differently. From the bench, the arrangement is the faithful execution of the constitutional design it was built to serve; from the electorate and the legislatures, the same structure operates as enforced exclusion from meaning-making — their considered judgments are reversible by a body they cannot discipline through ordinary politics. Rights-claimants occupy a third position: the arrangement is a lifeline that exists nowhere else for them. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Constitutional courts sit nearest the beneficiary end despite administering the arrangement: they collect the transferred authority directly, and their identity_locked exit deepens their structural investment. Rights-claimants derive the lowest directionality — full subsidy, trapped exit, no alternative channel. The legal professional elite sits low-to-moderate: genuine beneficiary, but its gains are derivative of the bench's primacy. Democratic electorates sit near the full-target end — trapped exit amplifies their exposure, since no mobility or arbitrage dampens the transfer. Elected legislatures are also targets, but their power and residual levers (appointments, jurisdiction proposals) place them slightly short of the electorates' exposure. Excluded voices are not seated in the derivation at all — their exclusion is the enforcement object, not a measured position within it.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling in both directions. A pure-coordination reading would erase the counter-majoritarian transfer — the demonstrable movement of decision power from elected bodies to an unelected bench — and would treat the foreclosure of override and departmentalism as mere housekeeping. A pure-extraction reading would erase the demonstrated coordination function: uniform constitutional meaning, minority protection that political channels repeatedly failed to deliver, and inter-branch deadlock resolution. The mandate (limit-enforcement and rights protection) is still live — corroborated by the historical record of majoritarian violation — so this is not an atrophied shell maintained by inertia, and the arrangement carries no sunset because its justification is steady-state, not transitional. Hence tangled_rope: genuine coordination and asymmetric extraction through the same structure, held up by active enforcement. The R5 mismatch read (status=live x verdict=world_rearranges) returns no zombie flag, consistent with the computed path.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading of the constitutional_text kernel — the judicial_supremacy_reading. What would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Comparative analysis of the sibling stories (legislative_sovereignty_reading, popular_sovereignty_reading): the legislative reading adds an override path, converting the payer seats into partial agenda-setters and collapsing the gatekeeper structure; the popular reading relocates final authority to amendment and convention, dissolving the court''s agenda-setter role entirely. The disagreement is located in one structural element: which institution''s determination of constitutional meaning is conclusive.',
    'Under the legislative reading the victim set shrinks (override restores democratic responsiveness) and enforcement shifts to political compliance; under the popular reading both the beneficiary concentration in courts and the gatekeeper function disappear. The classification of this file applies only to the judicial-supremacy instantiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: one of three readings of the constitutional_text kernel; siblings instantiate different constraints.').

omega_variable(
    finality_necessity_vs_allocation,
    'Is final interpretive authority a structural necessity of any written constitution (some institution must conclusively apply the text), or a constructed allocation concentrating power in identifiable beneficiaries?',
    'Examine systems that distribute finality differently — legislative override clauses, departmentalist practice, popular amendment triggers — and ask whether constitutional order collapses or merely reallocates. If order persists under distributed finality, necessity fails as justification and the arrangement reads as constructed.',
    'If finality is necessary, part of the measured extraction is the irreducible price of constitutional government; if constructed, the full extraction is attributable to the allocation choice and the beneficiary structure becomes decisive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(finality_necessity_vs_allocation, conceptual, 'Whether judicial finality is a natural feature of constitutional government or a contingent, benefit-conferring construction.').

omega_variable(
    democratic_cost_valuation,
    'Does judicial invalidation impose a net cost on democratic self-government, or does it protect the preconditions of democracy (participation rights of entrenched minorities, fair electoral rules) such that the victim framing misdescribes the transfer?',
    'Separate invalidations by object — those striking rights-restricting legislation versus those striking democracy-structuring legislation — and measure longitudinal effects on participation and responsiveness in each class.',
    'If most invalidation protects democratic preconditions, the payer-seat extraction estimate falls and the arrangement moves toward the coordination end; if invalidation routinely displaces substantive majoritarian judgment, the extraction stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(democratic_cost_valuation, preference, 'Valuation dispute over whether the democratic cost of judicial review is a harm or a democratic investment.').

omega_variable(
    formal_rigidity_vs_doctrinal_mobility,
    'Does judicial supremacy actually produce high rigidity in constitutional meaning, or does judge-made doctrine evolve faster than amendment-based systems change their texts?',
    'Compare rates of effective constitutional change: frequency and magnitude of doctrinal reversals and reinterpretations under judicial supremacy versus frequencies of formal amendment under legislative- or popular-finality systems.',
    'If doctrinal mobility is high, the high-rigidity expectation attached to this reading is descriptively false and the constraint''s stability claims weaken; the extraction profile shifts from freezing meaning toward monopolizing its revision.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(formal_rigidity_vs_doctrinal_mobility, empirical, 'Whether the arrangement rigidifies constitutional meaning or monopolizes a fast-moving interpretive process.').

omega_variable(
    litigant_resource_asymmetry,
    'Do rights-claimants benefit uniformly from judicial supremacy, or does the arrangement systematically favor repeat players with resources, converting the declared beneficiary group into a stratified set?',
    'Track constitutional dockets by litigant type and resources over time; compare win rates and remedy depth for organized repeat players versus one-shot resource-poor claimants.',
    'If benefits concentrate in resource-rich litigants, the beneficiary declaration splits and part of the arrangement reads as professional-class advantage riding on rights protection.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(litigant_resource_asymmetry, empirical, 'Heterogeneity of the beneficiary class under litigation-cost gating.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(constitutional_text__judicial_supremacy_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cons_tr_t0, constitutional_text__judicial_supremacy_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(cons_tr_t0, observed).
narrative_ontology:measurement(cons_tr_t10, constitutional_text__judicial_supremacy_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(cons_tr_t10, observed).
narrative_ontology:measurement(cons_tr_t20, constitutional_text__judicial_supremacy_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement_basis(cons_tr_t20, observed).
narrative_ontology:measurement(cons_tr_t30, constitutional_text__judicial_supremacy_reading, theater_ratio, 30, 0.23).
narrative_ontology:measurement_basis(cons_tr_t30, observed).
narrative_ontology:measurement(cons_tr_t40, constitutional_text__judicial_supremacy_reading, theater_ratio, 40, 0.26).
narrative_ontology:measurement_basis(cons_tr_t40, observed).
narrative_ontology:measurement(cons_tr_t50, constitutional_text__judicial_supremacy_reading, theater_ratio, 50, 0.27).
narrative_ontology:measurement_basis(cons_tr_t50, observed).
narrative_ontology:measurement(cons_tr_t60, constitutional_text__judicial_supremacy_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement_basis(cons_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(cons_be_t0, constitutional_text__judicial_supremacy_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(cons_be_t0, observed).
narrative_ontology:measurement(cons_be_t10, constitutional_text__judicial_supremacy_reading, base_extractiveness, 10, 0.44).
narrative_ontology:measurement_basis(cons_be_t10, observed).
narrative_ontology:measurement(cons_be_t20, constitutional_text__judicial_supremacy_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement_basis(cons_be_t20, observed).
narrative_ontology:measurement(cons_be_t30, constitutional_text__judicial_supremacy_reading, base_extractiveness, 30, 0.51).
narrative_ontology:measurement_basis(cons_be_t30, observed).
narrative_ontology:measurement(cons_be_t40, constitutional_text__judicial_supremacy_reading, base_extractiveness, 40, 0.54).
narrative_ontology:measurement_basis(cons_be_t40, observed).
narrative_ontology:measurement(cons_be_t50, constitutional_text__judicial_supremacy_reading, base_extractiveness, 50, 0.55).
narrative_ontology:measurement_basis(cons_be_t50, observed).
narrative_ontology:measurement(cons_be_t60, constitutional_text__judicial_supremacy_reading, base_extractiveness, 60, 0.56).
narrative_ontology:measurement_basis(cons_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(cons_su_t0, constitutional_text__judicial_supremacy_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(cons_su_t0, observed).
narrative_ontology:measurement(cons_su_t10, constitutional_text__judicial_supremacy_reading, suppression_requirement, 10, 0.5).
narrative_ontology:measurement_basis(cons_su_t10, observed).
narrative_ontology:measurement(cons_su_t20, constitutional_text__judicial_supremacy_reading, suppression_requirement, 20, 0.54).
narrative_ontology:measurement_basis(cons_su_t20, observed).
narrative_ontology:measurement(cons_su_t30, constitutional_text__judicial_supremacy_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement_basis(cons_su_t30, observed).
narrative_ontology:measurement(cons_su_t40, constitutional_text__judicial_supremacy_reading, suppression_requirement, 40, 0.61).
narrative_ontology:measurement_basis(cons_su_t40, observed).
narrative_ontology:measurement(cons_su_t50, constitutional_text__judicial_supremacy_reading, suppression_requirement, 50, 0.63).
narrative_ontology:measurement_basis(cons_su_t50, observed).
narrative_ontology:measurement(cons_su_t60, constitutional_text__judicial_supremacy_reading, suppression_requirement, 60, 0.65).
narrative_ontology:measurement_basis(cons_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(constitutional_text__judicial_supremacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, legislative_sovereignty_reading).
narrative_ontology:affects_constraint(constitutional_text__judicial_supremacy_reading, popular_sovereignty_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'constitutional interpretation' decomposes into three structurally distinct constraints — one per reading of the constitutional_text kernel. Each allocates final interpretive authority differently and therefore carries its own epsilon, beneficiary set, and victim set: this file instantiates the judicial-supremacy allocation (gatekeeper courts, no override); legislative_sovereignty_reading instantiates the parliamentary-override allocation; popular_sovereignty_reading instantiates constituent-power finality. The readings are linked, not merged: this reading's operation changes the legitimacy conditions and resource availability for the other two — a functioning override clause revives the legislative reading, and amendment or convention mobilization revives the popular one — which is why the edges run from this story to both siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
