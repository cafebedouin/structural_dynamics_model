% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__partnership_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__partnership_reading, []).

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
 *   constraint_id: waitangi_sovereignty_allocation__partnership_reading
 *   human_readable: Treaty of Waitangi as Ongoing Crown-Māori Partnership (Principles Doctrine)
 *   domain: constitutional/indigenous_rights
 *
 * SUMMARY:
 *   This constraint instantiates the partnership reading of the Waitangi
 *   kernel: the Treaty is understood as founding an ongoing, textually
 *   ambiguous Crown-Māori relationship of partnership, obliging the Crown to
 *   consult in good faith and actively protect Māori interests, formalized
 *   through the judicially developed 'principles of the Treaty' doctrine
 *   since the 1987 Lands case. This reading occupies a moderating middle
 *   position — it does not read Article I as complete, unconditioned cession
 *   (the crown_sovereignty_reading) nor does it read Article II as preserving
 *   full rangatiratanga with the Crown limited to kāwanatanga over settlers
 *   only (the rangatiratanga_reading). Instead it treats the ambiguity itself
 *   as generative of an ongoing fiduciary-like obligation, operationalized
 *   through the Waitangi Tribunal and negotiated settlements, while leaving
 *   parliamentary sovereignty formally intact and settlements capped and
 *   final. The extraction under this reading is moderate: real redress flows,
 *   but it is capped, delayed, and administered through a process the Crown
 *   itself controls and can legislate to narrow.
 *
 * KEY AGENTS:
 *   - crown_executive_and_legislature: agenda-setter and structural beneficiary of a legitimating framework it also funds and can narrow by ordinary legislation
 *   - iwi_and_hapu_claimants: primary payers and secondary beneficiaries — receive capped redress in exchange for finality clauses that foreclose future claims
 *   - non_settled_hapu_awaiting_redress: powerless payers with no settlement pathway, bearing costs without the partial benefits settled groups receive
 *   - settler_descendant_population: diffuse beneficiary retaining the great majority of transferred land and resource wealth
 *   - waitangi_tribunal: institutional observer whose recommendations are advisory absent Crown political will
 *   - future_courts_and_judiciary: institutional observer whose principles doctrine constrains but does not override statute
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__partnership_reading, 0.52).
domain_priors:suppression_score(waitangi_sovereignty_allocation__partnership_reading, 0.44).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__partnership_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 0.44).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__partnership_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__partnership_reading, "Treaty of Waitangi as Ongoing Crown-Māori Partnership (Principles Doctrine)").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__partnership_reading, "constitutional/indigenous_rights").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__partnership_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__partnership_reading, 'f120d113-01a4-4f93-b916-a8d855c460d5').
narrative_ontology:cs_kernel_codification('f120d113-01a4-4f93-b916-a8d855c460d5', distributed).
narrative_ontology:cs_authority_grounding('f120d113-01a4-4f93-b916-a8d855c460d5', practice).
narrative_ontology:cs_interpretation_layer_present('f120d113-01a4-4f93-b916-a8d855c460d5').
narrative_ontology:cs_reading_relation('f120d113-01a4-4f93-b916-a8d855c460d5', waitangi_sovereignty_allocation__crown_sovereignty_reading, influences).
narrative_ontology:cs_reading_relation('f120d113-01a4-4f93-b916-a8d855c460d5', waitangi_sovereignty_allocation__rangatiratanga_reading, influences).
narrative_ontology:cs_axiom('f120d113-01a4-4f93-b916-a8d855c460d5', foundational, textual_ambiguity_generates_fiduciary_obligation).
narrative_ontology:cs_axiom_status(textual_ambiguity_generates_fiduciary_obligation, holdable).
narrative_ontology:cs_axiom_grounding('f120d113-01a4-4f93-b916-a8d855c460d5', textual_ambiguity_generates_fiduciary_obligation, conventional).
narrative_ontology:cs_axiom('f120d113-01a4-4f93-b916-a8d855c460d5', foundational, consultation_and_redress_constrain_without_overriding_parliamentary_supremacy).
narrative_ontology:cs_axiom_status(consultation_and_redress_constrain_without_overriding_parliamentary_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('f120d113-01a4-4f93-b916-a8d855c460d5', consultation_and_redress_constrain_without_overriding_parliamentary_supremacy, conventional).
narrative_ontology:cs_reference_frame('f120d113-01a4-4f93-b916-a8d855c460d5', treaty_text_as_living_constitutional_instrument).
narrative_ontology:cs_drift_state('f120d113-01a4-4f93-b916-a8d855c460d5', post_1987_lands_case_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('f120d113-01a4-4f93-b916-a8d855c460d5', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__partnership_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, crown_executive_and_legislature).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, settler_descendant_population).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, iwi_and_hapu_claimants).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, iwi_and_hapu_claimants).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, non_settled_hapu_awaiting_redress).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__partnership_reading, treaty_principles_doctrine).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__partnership_reading, partnership_analogy_constitutional_status).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds parliamentary sovereignty and legislates the Treaty's legal effect (via specific incorporating statutes and Waitangi Tribunal jurisdiction); decides which principles to codify, which settlements to fund, and retains the power to override or narrow tribunal findings through ordinary legislation. Gains legitimacy and reduced political-risk exposure from framing governance as consultative partnership rather than unilateral rule.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, crown_executive_and_legislature, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__partnership_reading, crown_executive_and_legislature, beneficiary).

% Bring claims through the Waitangi Tribunal and negotiate settlements framed as full and final redress, often for a fraction of asset value lost. Gains include capped financial and cultural redress, formal Crown apology, and enhanced consultation rights; costs include settlement caps that foreclose future claims, prolonged negotiation timelines measured in decades, and continued subordination of rangatiratanga claims to Crown sovereignty as the ultimate legal frame.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, iwi_and_hapu_claimants, payer,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__partnership_reading, iwi_and_hapu_claimants, beneficiary).

% Groups whose historical claims remain unresolved or contested (overlapping mandate disputes, unrecognized hapu status) sit outside the negotiated settlement pipeline entirely. They bear the ongoing costs of dispossession without the partial redress settled groups receive, and have no direct lever to accelerate Crown engagement.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, non_settled_hapu_awaiting_redress, payer,
    powerless, generational, trapped, regional).

% Continues to hold the vast majority of land and resource wealth transferred under colonial settlement; benefits from the partnership framing's legitimating function (a settled, judicially recognized process substitutes for open-ended historical liability) and from finality clauses embedded in negotiated settlements.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, settler_descendant_population, beneficiary,
    organized, generational, mobile, national).

% A permanent commission of inquiry that investigates claims and issues recommendations interpreting Treaty principles; it has no binding power to compel Crown action on most land currently in Crown ownership and none over private land, so its findings depend on political will for implementation.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% Common-law courts that have developed the 'principles of the Treaty' doctrine (Lands case 1987 onward) as an interpretive gloss read into statutes referencing the Treaty; their doctrine constrains executive and legislative action only where Parliament has chosen to incorporate Treaty reference, and can be legislated around.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, future_courts_and_judiciary, observer,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a durable, judicially administrable framework through which Crown and Māori interests are reconciled without requiring either a full transfer of sovereignty or its wholesale denial — allowing ongoing governance, land administration, and resource allocation to proceed with a recognized (if contested) channel for grievance and consultation.
% TRANSFER_FUNCTION: Moves partial, capped financial and cultural redress from the Crown (funded by the general taxpayer) to specific negotiated iwi/hapu groups, while moving political and reputational legitimacy from the redress process back to the Crown; land and resource wealth transferred at colonization remains substantially with the Crown and settler-descendant population rather than being returned.
% ABSENT_VOICES: Hapu and iwi with unresolved or contested mandate status are structurally outside the negotiation table until the Crown recognizes their claim as ripe; they would argue the partnership framing legitimizes indefinite deferral. Advocates of the rangatiratanga reading would argue the entire consultation/redress apparatus concedes too much Crown authority at the outset by treating kāwanatanga as full sovereignty rather than limited governorship.
% DISAPPEARANCE_RATIONALE: If the principles doctrine and the consultation/settlement apparatus vanished overnight, negotiated settlements would lose their legal anchor, the Waitangi Tribunal's interpretive authority would dissolve, and the Crown would face renewed litigation and political pressure without any established mechanism for redress — the current, comparatively stable settlement pipeline would collapse into ad hoc political negotiation or unmediated conflict.
% FOUNDING_PROBLEM: The 1840 Treaty text is ambiguous and internally inconsistent between its English and Māori versions regarding the scope of authority ceded to the Crown, and 19th-20th century Crown practice largely ignored Treaty obligations; by the late 20th century this created accumulating, legally unaddressed grievances threatening both social stability and the Crown's constitutional legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: The Crown and the courts (via the Court of Appeal's 1987 Lands case and subsequent jurisprudence) attest the founding problem was substantially addressed by developing the principles doctrine and settlement process. Independent scholars, the Waitangi Tribunal's own reports, and iwi negotiators outside government attest the underlying problem — unresolved land loss and asymmetric sovereignty allocation — remains substantially live, since settlements are capped and full rangatiratanga claims remain unresolved; this corroboration comes from outside the Crown as principal beneficiary.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__partnership_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__partnership_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__partnership_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__partnership_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__partnership_reading, 0.52, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__partnership_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__partnership_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(waitangi_sovereignty_allocation__partnership_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.52) is authored as moderate-to-substantial under this reading: real transfers occur (settlements, consultation rights, tribunal recognition) but they are structurally capped, and the party administering the process (the Crown) also funds and can legislatively narrow it. Suppression (0.44) reflects that alternatives to the negotiated-settlement pathway are not fully foreclosed — litigation, direct political action, and international human-rights forums remain theoretically available, though practically constrained by resource asymmetry. Theater ratio (0.4) reflects a genuine but partial coordination function: the principles doctrine does real interpretive work, but a meaningful share of consultation activity is procedural box-ticking that produces engagement records without altering underlying resource allocation. Accessibility collapse is low-moderate (0.35) because rangatiratanga and crown-sovereignty framings remain live alternative readings actively argued in courts and politics — this reading has not achieved total interpretive closure. Resistance is moderately high (0.6), reflecting ongoing iwi advocacy for stronger rangatiratanga recognition and periodic Crown political pushback against expanding principles jurisprudence.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown's seat this reading looks like genuine, hard-won coordination — a stable constitutional accommodation replacing decades of unaddressed grievance with a workable process. From unsettled or under-compensated iwi/hapu seats, the same doctrine looks like a mechanism that legitimates continued Crown control over the pace and scope of redress while formally gesturing at partnership. The engine should register this as tangled_rope: coordination function is real (a stable, judicially administered channel exists) and asymmetric extraction is also real (redress capped, doctrine narrowable by the party it constrains), both riding the same structure and requiring active enforcement (tribunal processes, incorporating statutes, settlement legislation) to persist.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown sits nearest the beneficiary end: it designed the incorporating statutes, funds settlements from general revenue, and retains ultimate legislative control over the doctrine's scope — d is derived low. Settled iwi/hapu occupy a mixed position: real redress flows to them (moderating d downward from full-target) but the redress is capped and finality-clause-bound, and they bear the cost of the underlying historical dispossession the settlement only partially remedies (pulling d back upward) — hence dual role payer/beneficiary. Non-settled hapu are trapped payers with no settlement channel and the highest derived d. Settler-descendants are diffuse beneficiaries who retain transferred wealth without direct participation in the consultation/redress machinery.
 *
 * MANDATROPHY ANALYSIS:
 *   The partnership reading resists mandatrophy misclassification in both directions: treating it as pure extraction (snare) would erase the real, non-trivial redress and consultation gains iwi have won through the Tribunal and courts since 1975; treating it as pure coordination (rope) would erase the fact that the Crown retains the power to legislate the doctrine narrower at any time and that settlements are structured to extinguish future claims. Tangled_rope captures the genuine dual structure: coordination function (a working, precedent-respecting dispute-resolution channel) coexists with asymmetric extraction (caps, finality, Crown-controlled scope) through the same doctrinal apparatus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    principles_doctrine_constitutional_weight,
    'Does the judicially developed ''principles of the Treaty'' doctrine constitute a genuine constraint on parliamentary sovereignty, or is it fully defeasible by any Parliament that chooses to legislate contrary to it?',
    'Track instances where Parliament has explicitly overridden or excluded Treaty principles language in legislation (e.g., foreshore and seabed legislation history) versus instances where courts have used the doctrine to compel Crown action against its stated preference.',
    'If the doctrine is fully defeasible in practice, this reading''s coordination function is closer to theater than genuine constraint, pushing the classification toward snare; if courts have meaningfully constrained Crown action against its preference, the coordination function is more substantial, supporting the tangled_rope reading as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(principles_doctrine_constitutional_weight, empirical, 'Whether the principles doctrine binds or merely gestures.').

omega_variable(
    settlement_finality_versus_ongoing_grievance,
    'Do ''full and final'' settlement clauses genuinely resolve the underlying historical grievance, or do they suppress the expression of continuing grievance without resolving its substance?',
    'Longitudinal survey of settled iwi/hapu satisfaction and re-litigation attempts; comparison of settled asset values to independently assessed historical loss valuations.',
    'If finality clauses substantially undervalue historical loss, the measured extractiveness under this reading should be revised upward and the suppression figure understates internalized grievance suppression among settled groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_finality_versus_ongoing_grievance, empirical, 'Whether settlement finality reflects genuine resolution or suppressed grievance.').

omega_variable(
    framing_choice_partnership_versus_alternatives,
    'Is the partnership framing itself a defensible middle reading of the 1840 text, or a judicially and politically convenient compromise that avoids adjudicating the harder textual questions the other two readings force?',
    'Comparative textual and historical analysis of the Māori and English texts against the doctrinal history of how courts arrived at ''partnership'' language versus direct engagement with rangatiratanga/kāwanatanga distinctions.',
    'If partnership is best understood as an avoidance strategy rather than a principled textual reading, this reading''s own coordination-function claim weakens, and its extraction profile may be understated relative to a reading that more directly confronts the sovereignty question.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framing_choice_partnership_versus_alternatives, conceptual, 'Whether the partnership reading is a genuine textual reading or an institutionally convenient compromise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__partnership_reading, 1975, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t1975, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 1975, 0.55).
narrative_ontology:measurement(wait_tr_t1985, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 1985, 0.5).
narrative_ontology:measurement(wait_tr_t1995, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 1995, 0.45).
narrative_ontology:measurement(wait_tr_t2005, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 2005, 0.42).
narrative_ontology:measurement(wait_tr_t2015, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 2015, 0.4).
narrative_ontology:measurement(wait_tr_t2025, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(wait_be_t1975, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 1975, 0.68).
narrative_ontology:measurement(wait_be_t1985, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 1985, 0.6).
narrative_ontology:measurement(wait_be_t1995, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 1995, 0.56).
narrative_ontology:measurement(wait_be_t2005, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 2005, 0.54).
narrative_ontology:measurement(wait_be_t2015, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 2015, 0.53).
narrative_ontology:measurement(wait_be_t2025, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 2025, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t1975, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 1975, 0.65).
narrative_ontology:measurement(wait_su_t1985, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 1985, 0.55).
narrative_ontology:measurement(wait_su_t1995, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 1995, 0.5).
narrative_ontology:measurement(wait_su_t2005, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 2005, 0.47).
narrative_ontology:measurement(wait_su_t2015, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 2015, 0.45).
narrative_ontology:measurement(wait_su_t2025, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 2025, 0.44).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__partnership_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, crown_sovereignty_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, rangatiratanga_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the waitangi_sovereignty_allocation kernel. crown_sovereignty_reading treats Article I as complete cession under Westminster parliamentary supremacy (lower authored extraction from the Crown's own vantage, higher from a rangatiratanga vantage). rangatiratanga_reading treats the Māori text as retaining full authority over lands/resources/taonga with only kāwanatanga ceded (highest authored extraction, since it treats nearly all subsequent Crown land administration as unauthorized). This partnership_reading occupies the doctrinal middle ground actually operationalized in New Zealand law since 1987, with moderate authored extraction reflecting real but capped redress. Each story carries its own ε and stakeholder structure per the ε-invariance principle; do not average across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
