% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__living_document_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__living_document_reading, []).

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
 *   constraint_id: magna_carta_1215__living_document_reading
 *   human_readable: Magna Carta as Living Constitutional Document (Interpretive Tradition Reading)
 *   domain: constitutional/legal/political
 *
 * SUMMARY:
 *   Magna Carta (1215) presents a contested constitutional kernel. This story
 *   instantiates the LIVING-DOCUMENT READING: the constraint is understood as
 *   an adaptive constitutional substrate whose original feudal meaning has
 *   been legitimately superseded by centuries of judicial precedent and
 *   doctrinal interpretation. The constraint's function is to provide a
 *   canonical text that judges, scholars, and legislators use to anchor
 *   constitutional development without formal amendment. Original meaning
 *   binds initially, but precedential accumulation constitutes legitimate
 *   constitutional evolution. The claim/metric divergence is intentional:
 *   this reading is CLAIMED as rope (genuine coordination problem: constrain
 *   arbitrary power; solution: binding legality norm) while the authored
 *   metrics describe modest extractiveness (0.38) and theater (0.31),
 *   reflecting that the reading benefits a specific institutional seat
 *   (common-law judges, constitutional scholars) while delivering genuine
 *   protections to citizens. The engine measures this divergence; do not
 *   reconcile.
 *
 * KEY AGENTS:
 *   - Common-law judges: institutional agenda-setter; interpret Magna Carta through precedent and adapt it to new circumstances; extract authority to adjudicate novel rights claims; d ≈ 0.3 (beneficiary of interpretive authority)
 *   - Constitutional scholars: institutional beneficiary; develop and legitimize the living-document doctrine through treatises and commentary; secure professional authority; d ≈ 0.2 (beneficiary of scholarship's essentiality to the system)
 *   - Parliaments/legislatures: institutional, mixed position; operate within expanded constitutional constraints derived from interpretation; d ≈ 0.5 (symmetric: benefit from flexibility, constrained by precedent)
 *   - Citizens/rights claimants: organized beneficiaries of expanded protections; depend on judges/scholars to recognize their claims; d ≈ 0.65 (net beneficiaries but excluded from interpretive process)
 *   - Originalist judges/scholars: institutional excluded; hold competing reading; d ≈ 0.75 (targets of the living-document authority structure; their interpretive methodology is delegitimized)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__living_document_reading, 0.38).
domain_priors:suppression_score(magna_carta_1215__living_document_reading, 0.22).
domain_priors:theater_ratio(magna_carta_1215__living_document_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__living_document_reading, rope).
narrative_ontology:human_readable(magna_carta_1215__living_document_reading, "Magna Carta as Living Constitutional Document (Interpretive Tradition Reading)").
narrative_ontology:topic_domain(magna_carta_1215__living_document_reading, "constitutional/legal/political").

domain_priors:requires_active_enforcement(magna_carta_1215__living_document_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__living_document_reading, '5902deb1-acf3-439d-a428-2f7af529591c').
narrative_ontology:cs_kernel_codification('5902deb1-acf3-439d-a428-2f7af529591c', fixed_text).
narrative_ontology:cs_authority_grounding('5902deb1-acf3-439d-a428-2f7af529591c', lineage).
narrative_ontology:cs_interpretation_layer_present('5902deb1-acf3-439d-a428-2f7af529591c').
narrative_ontology:cs_reading_relation('5902deb1-acf3-439d-a428-2f7af529591c', magna_carta_1215__baronial_privilege_reading, coexists_with).
narrative_ontology:cs_reading_relation('5902deb1-acf3-439d-a428-2f7af529591c', magna_carta_1215__universal_rights_reading, coexists_with).
narrative_ontology:cs_axiom('5902deb1-acf3-439d-a428-2f7af529591c', foundational, precedential_accumulation_constitutes_legitimate_development).
narrative_ontology:cs_axiom_status(precedential_accumulation_constitutes_legitimate_development, holdable).
narrative_ontology:cs_axiom_grounding('5902deb1-acf3-439d-a428-2f7af529591c', precedential_accumulation_constitutes_legitimate_development, conventional).
narrative_ontology:cs_axiom('5902deb1-acf3-439d-a428-2f7af529591c', foundational, original_meaning_binding_but_supersedable_by_tradition).
narrative_ontology:cs_axiom_status(original_meaning_binding_but_supersedable_by_tradition, holdable).
narrative_ontology:cs_axiom_grounding('5902deb1-acf3-439d-a428-2f7af529591c', original_meaning_binding_but_supersedable_by_tradition, deontological).
narrative_ontology:cs_reference_frame('5902deb1-acf3-439d-a428-2f7af529591c', feudal_contract_with_expanding_scope).
narrative_ontology:cs_drift_state('5902deb1-acf3-439d-a428-2f7af529591c', contemporary_constitutional_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('5902deb1-acf3-439d-a428-2f7af529591c', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__living_document_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, common_law_judges).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, constitutional_scholars_living_tradition).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, parliamentary_legislatures).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, citizens_under_expanded_protections).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, parliamentary_legislatures).
narrative_ontology:constraint_vindicates(magna_carta_1215__living_document_reading, constitutional_adaptability_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_1215__living_document_reading, precedential_accumulation_as_legitimate_development).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret Magna Carta through successive legal precedents and changing social conditions. They treat original meaning as foundational but superseded through legitimate doctrinal development. Each judgment extends the constraint's scope and application to novel circumstances, adding precedential layers that constitute constitutional evolution. Judges benefit from this framing because it grants them interpretive authority to adapt the constraint to contemporary needs without formally amending it.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, common_law_judges, agenda_setter,
    institutional, generational, analytical, national).

% Develop and transmit the living-document reading through treatises, commentaries, and academic discourse. They benefit by securing professional authority and influence over constitutional interpretation. The living-document framing privileges interpretive scholarship as the mechanism by which the Constitution stays relevant; scholarship becomes essential to the constraint's operation and reproduction.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, constitutional_scholars_living_tradition, beneficiary,
    institutional, generational, mobile, national).

% Operate within a constitutional framework whose original scope has been vastly expanded through interpretive tradition. They benefit from the flexibility this grants (acting on rights and constraints not textually present in 1215) but are also bound by interpretations they did not choose and cannot directly revise without formal amendment. The living-document reading makes them both beneficiaries of adaptability and constrained by precedential accumulation.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, parliamentary_legislatures, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__living_document_reading, parliamentary_legislatures, payer).

% Enjoy expanded protections derived from Magna Carta through centuries of interpretation: due process in modern criminal procedure, jury trial rights, property protections evolved far beyond 1215 meanings. The living-document reading extends the constraint to protect them; without it, Clause 39 (per legem terrae) would bind only feudal barons in specific disputes. They depend on the constraint but have no direct voice in how it is interpreted.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, citizens_under_expanded_protections, beneficiary,
    organized, biographical, constrained, national).

% Hold the competing reading that original meaning is binding and interpretive departure is illegitimate. They are excluded from the living-document authority structure because that structure treats their textual methodology as historically naive and lacking the sophistication to account for constitutional development. Their objections are heard in dissent and academic debate but do not set the constraint's operation.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, originalist_judges_and_scholars, excluded,
    institutional, generational, trapped, national).

% Seek protections grounded in Magna Carta for persons and circumstances that would not have been imagined in 1215 or recognized in any single prior precedent: prisoners' rights, digital privacy, welfare entitlements. They are technically included in the living-document reading's scope but excluded from the process by which their claims are evaluated and accepted or rejected. They depend on judges and scholars to recognize their claim as a legitimate next step in constitutional development, with no guarantee it will be granted.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, contemporary_rights_claimants_outside_established_scope, excluded,
    powerless, biographical, trapped, local).

% Document what Magna Carta actually meant to the barons of 1215 and track the divergence between original meaning and later interpretation. They take no position on which reading is legitimate; their role is to establish the factual record of how much interpretive departure has occurred and to preserve evidence of the original context.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, historians_of_original_intent, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_1215__living_document_reading, common_law_judges).
narrative_ontology:fixing_cost_class(magna_carta_1215__living_document_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework of legality constraining executive power and protecting named parties from arbitrary action. The living-document reading coordinates by treating earlier precedents as binding authorities that constrain present interpretation: each judgment depends on and extends prior judgments, creating a chain of legitimate development. Precedential accumulation is itself the coordination mechanism — it ensures interpretive change is cumulative, not arbitrary.
% TRANSFER_FUNCTION: Moves interpretive authority from the text itself (and its original meaning) to the evolving tradition of judicial precedent and doctrinal commentary. Judges and constitutional scholars receive authority to declare what Magna Carta means in new contexts; citizens receive expanded protections derived from that interpretive work; originalist judges and scholars pay the cost of exclusion from the interpretive authority structure.
% ABSENT_VOICES: Originalist judges and scholars who hold the competing baronial-privilege or textual-original-meaning readings are structurally excluded from the living-document authority apparatus — their objections are heard in dissent and academic debate but do not shape the constraint's operation. Medieval barons, who would contest the modern scope of protections claimed in their name, are absent by historical fact. Contemporary claimants whose rights would require novel interpretive steps (digital privacy, welfare entitlements) have no formal voice in whether their claims will be recognized as legitimate extensions of the tradition.
% DISAPPEARANCE_RATIONALE: If the living-document reading and its precedential authority structure disappeared overnight, constitutional law would face immediate bifurcation: courts would either revert to original meaning (baronial privilege reading), radically shrinking the scope of protected persons and rights, or adopt the explicit universal-rights reading, which would require wholesale reinterpreting Magna Carta as a transhistorical rights document. Either way, the intricate body of precedent linking 1215 to modern constitutional practice would lose its legitimizing narrative. Citizens would have to be told their protections rest on a different foundation (if any). The entire constitutional system's claim to continuity and organic development would rupture.
% FOUNDING_PROBLEM: Arbitrary royal power unconstrained by law; barons sought protection against the Crown's ability to take property, life, or liberty without due process. The original 1215 problem was narrow: feudal superiors (the King) exploiting their authority over feudal inferiors (the barons).
% FOUNDING_PROBLEM_CORROBORATION: Constitutional historians and originalist scholars attest that the founding problem has evolved beyond recognition: modern protections extend to all persons, not barons; arbitrary power now takes different forms (administrative state, legislative overreach) than royal feudal exaction. Living-document judges and scholars attest that the founding problem is perennially live in each new form — the constraint's function is to identify arbitrary power and constrain it, regardless of historical form. Legislators and rights claimants treat the evolved problem as the binding one. No party outside the benefiting interpretive tradition independently corroborates that precedential accumulation itself is a legitimate constitutional method (originalists deny it; historians document it but take no normative stance).
narrative_ontology:disappearance_verdict(magna_carta_1215__living_document_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__living_document_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__living_document_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(magna_carta_1215__living_document_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__living_document_reading_tests).
:- end_tests(magna_carta_1215__living_document_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is 0.38 (moderate-low) because the reading does extract interpretive authority from the text itself and from the broader body of citizens — the living-document framework privileges judicial and scholarly interpretation over direct citizen input or textual clarity. However, the extraction is bounded by the reading's own internal commitment to precedential continuity: judges cannot depart radically from prior precedent without losing legitimacy, so the extraction is constrained by the very tradition that justifies it. Suppression is low (0.22) because the reading does not require coercive exclusion of alternative readings — originalists and universal-rights scholars publish their criticisms openly; the living-document reading simply outweighs them in institutional practice. Theater is moderate (0.31) because a growing share of the constraint's operation is devoted to maintaining the appearance of organic constitutional development through precedent, even when the substantive expansion of rights outpaces judicial willingness to acknowledge how far interpretation has departed from original meaning. The measurement series spans 800 time units (1215 CE to contemporary era, with t=0 as 1215, t=800 as ~2015). Extractiveness and theater both rise monotonically as the constraint's scope expands and the gap between original and interpreted meaning widens — a signal that the reading's claim to organic development is increasingly performative.
 *
 * PERSPECTIVAL GAP:
 *   From the judges' and scholars' seat, the living-document reading is genuine coordination with built-in flexibility: a text that can adapt to new circumstances without formal amendment preserves constitutional legitimacy across generations. From the originalists' seat, the same structure is interpretive overreach and illegitimate departure from binding text. From citizens' and rights claimants' seat, the reading is beneficent (they receive protections) but opaque (they cannot predict or influence what protections will be recognized next). From the historians' seat, the reading is remarkable for how much interpretive distance has accumulated: Magna Carta in 1215 was a feudal contract; by 1689 (Bill of Rights) it had become a symbol of limited government; by the 20th century it had become the supposed fountainhead of universal rights. The engine computes these divergences from the structural data: beneficiary/victim declarations, exit options, and power differentials. The living-document reading claims to be rope (coordination) but the metrics show it extracting authority (modest but real extractiveness 0.38) from those excluded from interpretation.
 *
 * DIRECTIONALITY LOGIC:
 *   Common-law judges are beneficiaries of this reading: it grants them authority to interpret and reinterpret Magna Carta in light of changing circumstances, making their role essential to constitutional legitimacy. From their seat, the constraint is genuine coordination (constrain arbitrary power through binding legal norms) with the side benefit that this coordination requires their interpretive labor and expertise. Constitutional scholars similarly benefit: the living-document reading makes scholarship the mechanism by which constitutional meaning evolves, securing their professional authority. Citizens and rights claimants are net beneficiaries but with reduced directionality: they receive expanded protections but have no voice in how the constraint is interpreted; they depend on judges recognizing their claims. Originalist judges and scholars are the targets (d ≈ 0.75): the living-document reading delegitimizes their methodology and excludes them from the authority structure that interprets Magna Carta. They pay the cost of having their interpretation marginalized while still being bound by the precedents that accumulate from the living-document reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The living-document reading does not exhibit mandatrophy in the straightforward sense (that the constraint persists after its founding problem is solved). Instead, it exhibits a more subtle form: the founding problem has evolved and proliferated. The original 1215 problem (arbitrary royal power over feudal barons) has been superseded, but the constraint has been reinterpreted to address new forms of arbitrary power (administrative, legislative, etc.) that were not contemplated in the original context. The reading maintains legitimacy by claiming this evolution is organic and precedential, but the evolution has now dwarfed the original: modern rights protections extend to all persons, not barons; they address circumstances unknown in 1215. The constraint persists not because the original problem is still live, but because the reading has found new problems to solve and new beneficiaries to protect. This is not mandatrophy (the constraint's function did not atrophy) but rather constitutional mutation — the constraint's function has expanded so far beyond its original moorings that the claim to continuity becomes increasingly theatrical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    original_meaning_vs_developed_meaning_boundary,
    'Where is the boundary between legitimate interpretive development and illegitimate departure from original meaning? What would count as interpretive extension too far to claim fidelity to Magna Carta?',
    'The living-document reading offers no internal criterion for this boundary — it treats continuity as preserved by precedential accumulation, not by fidelity to original text. Resolution would require either adopting originalism (text binds, deviation is illegitimate) or universal-rights reading (the aspirational meaning is the true one). Those are different constraints; this reading cannot settle the question without self-refutation.',
    'If the boundary is discovered to be arbitrary (precedent alone does not distinguish legitimate from illegitimate development), the constraint''s legitimacy undergoes reclassification from rope (genuine coordination) to tangled_rope (extraction masked as coordination): judges extract authority by claiming continuity is preserved when substantive meaning has radically changed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(original_meaning_vs_developed_meaning_boundary, conceptual, 'Boundary ambiguity between interpretive development and departure from original text.').

omega_variable(
    precedent_as_legitimate_authority_source,
    'Is precedential accumulation (the fact that prior judges interpreted Magna Carta a certain way) a legitimate source of constitutional authority, or is it merely historical happenstance that has been reified into legitimacy?',
    'Normative argument from philosophy of law and jurisprudence. No empirical resolution; this is a question of whether we grant precedent binding force or treat it as advisory. Different legal traditions (common law vs. civil law) answer differently.',
    'If precedent is not a legitimate authority source (merely historical), the constraint reverts to dependence on the text itself (originalism) or on explicit legal amendment (positive law). The living-document reading collapses and the constraint bifurcates into competing readings with no meta-framework to coordinate them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_as_legitimate_authority_source, preference, 'Whether accumulated precedent is normatively binding or merely historically contingent.').

omega_variable(
    scope_expansion_as_intentional_or_drift,
    'Does the expansion of Magna Carta''s scope from feudal contract to universal-rights foundation represent intentional constitutional development by the judicial tradition, or is it an unacknowledged drift in which the text''s meaning has drifted far from what courts would honestly claim?',
    'Historical analysis of judicial opinions: do judges acknowledge the scope expansion, or do they claim continuity while expanding scope? If acknowledged, the expansion is intentional; if unacknowledged, it is drift masquerading as continuity.',
    'If drift is extensive and unacknowledged, the constraint''s theater_ratio should be higher (performative maintenance of the appearance of continuity masks actual substitution of meaning), and the extractiveness should be reclassified: judges are extracting authority not through transparent reinterpretation but through misrepresentation of continuity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_expansion_as_intentional_or_drift, empirical, 'Whether judicial scope expansion is transparent or masked by false claims of continuity.').

omega_variable(
    kernel_reading_interdependence,
    'This reading (living-document) depends on treating Magna Carta as a flexible constitutional substrate. But does this reading foreclose the baronial-privilege reading, or merely marginalize it as a legitimate alternative?',
    'Test whether a judge could simultaneously hold the living-document framework AND treat Magna Carta as binding only the original contracting parties (barons). If such a hybrid is logically possible, the readings coexist; if not, the living-document reading structurally forecloses baronial-privilege by redefining what Magna Carta is (a constitution, not a feudal contract). This is not a foreclosure by argument but by framework redefinition.',
    'If the living-document reading structurally forecloses the baronial-privilege reading, the reading_relations entry for baronial_privilege should be forecloses, not coexists_with. The sibling readings would not genuinely coexist; the living-document reading would be the dominant meta-framework that permits or denies legitimacy to the others.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_interdependence, conceptual, 'Whether readings coexist as live positions or whether the living-document framework forecloses alternatives through redefinition.').

omega_variable(
    citizens_as_excluded_or_beneficiaries,
    'Are contemporary citizens who benefit from expanded Magna Carta protections beneficiaries of the constraint, or are they payers whose need for judicial interpretation of their rights represents an extraction of authority from them to judges and scholars?',
    'Examine whether citizens have alternative paths to rights protection (legislation, direct constitutional amendment) that would make judicial interpretation of Magna Carta supplementary, or whether judicial interpretation is their primary path. If supplementary, they benefit without being extracted from. If primary, they are dependent on judges'' interpretive grace.',
    'If citizens are primarily dependent on judicial interpretation, they should be classified as payers (they pay the cost of exclusion from the interpretive process) not pure beneficiaries. This would raise the constraint''s extractiveness: a larger portion of the measured extraction would flow from citizens to judges rather than from the measured victims (payers in the declared structure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(citizens_as_excluded_or_beneficiaries, empirical, 'Whether citizen dependence on judicial interpretation of Magna Carta is extractive or beneficial.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__living_document_reading, 0, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t0, magna_carta_1215__living_document_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(magn_tr_t0, projected).
narrative_ontology:measurement(magn_tr_t100, magna_carta_1215__living_document_reading, theater_ratio, 100, 0.12).
narrative_ontology:measurement_basis(magn_tr_t100, observed).
narrative_ontology:measurement(magn_tr_t200, magna_carta_1215__living_document_reading, theater_ratio, 200, 0.16).
narrative_ontology:measurement_basis(magn_tr_t200, observed).
narrative_ontology:measurement(magn_tr_t400, magna_carta_1215__living_document_reading, theater_ratio, 400, 0.25).
narrative_ontology:measurement_basis(magn_tr_t400, observed).
narrative_ontology:measurement(magn_tr_t600, magna_carta_1215__living_document_reading, theater_ratio, 600, 0.29).
narrative_ontology:measurement_basis(magn_tr_t600, observed).
narrative_ontology:measurement(magn_tr_t800, magna_carta_1215__living_document_reading, theater_ratio, 800, 0.31).
narrative_ontology:measurement_basis(magn_tr_t800, observed).

% Extraction over time
narrative_ontology:measurement(magn_be_t0, magna_carta_1215__living_document_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement_basis(magn_be_t0, projected).
narrative_ontology:measurement(magn_be_t100, magna_carta_1215__living_document_reading, base_extractiveness, 100, 0.18).
narrative_ontology:measurement_basis(magn_be_t100, observed).
narrative_ontology:measurement(magn_be_t200, magna_carta_1215__living_document_reading, base_extractiveness, 200, 0.26).
narrative_ontology:measurement_basis(magn_be_t200, observed).
narrative_ontology:measurement(magn_be_t400, magna_carta_1215__living_document_reading, base_extractiveness, 400, 0.35).
narrative_ontology:measurement_basis(magn_be_t400, observed).
narrative_ontology:measurement(magn_be_t600, magna_carta_1215__living_document_reading, base_extractiveness, 600, 0.37).
narrative_ontology:measurement_basis(magn_be_t600, observed).
narrative_ontology:measurement(magn_be_t800, magna_carta_1215__living_document_reading, base_extractiveness, 800, 0.38).
narrative_ontology:measurement_basis(magn_be_t800, observed).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t0, magna_carta_1215__living_document_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(magn_su_t0, projected).
narrative_ontology:measurement(magn_su_t100, magna_carta_1215__living_document_reading, suppression_requirement, 100, 0.18).
narrative_ontology:measurement_basis(magn_su_t100, observed).
narrative_ontology:measurement(magn_su_t200, magna_carta_1215__living_document_reading, suppression_requirement, 200, 0.19).
narrative_ontology:measurement_basis(magn_su_t200, observed).
narrative_ontology:measurement(magn_su_t400, magna_carta_1215__living_document_reading, suppression_requirement, 400, 0.21).
narrative_ontology:measurement_basis(magn_su_t400, observed).
narrative_ontology:measurement(magn_su_t600, magna_carta_1215__living_document_reading, suppression_requirement, 600, 0.22).
narrative_ontology:measurement_basis(magn_su_t600, observed).
narrative_ontology:measurement(magn_su_t800, magna_carta_1215__living_document_reading, suppression_requirement, 800, 0.22).
narrative_ontology:measurement_basis(magn_su_t800, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__living_document_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(magna_carta_1215__living_document_reading, 0.18).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, magna_carta_1215__baronial_privilege_reading).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, magna_carta_1215__universal_rights_reading).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, common_law_precedent_as_constitutional_authority).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, judicial_interpretive_authority_in_common_law_systems).

% DUAL FORMULATION NOTE:
% The Magna Carta kernel decomposes into three distinct constraint stories, each instantiating a reading with different ε values, beneficiary/victim structures, and types. The living-document reading (this story) treats Magna Carta as a binding text whose meaning evolves through precedent; it is claimed as rope (coordination problem: constrain arbitrary power) but shows modest extractiveness (0.38) reflecting judges' authority benefit. The baronial-privilege reading treats Magna Carta as a feudal contract with limited scope; it is a mountain (natural law: feudal relations were what they were) or snare (extraction: the constraints protected elite property). The universal-rights reading treats Magna Carta as transhistorical fountain of human rights; it is either rope (genuine coordination: establish that all humans have basic rights) or scaffold (transitional: developing toward a more just system). These readings are not perspectives on one constraint; they are three separate constraints sharing a textual kernel but instantiating different structural relationships. The living-document reading influences the others by establishing the meta-framework in which Magna Carta's meaning is adjudicated; it coexists with baronial-privilege (held by originalists) and with universal-rights (held by human-rights advocates), but the framework itself scaffolds the contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(magna_carta_1215__living_document_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
