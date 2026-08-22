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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: waitangi_sovereignty_allocation__partnership_reading
 *   human_readable: Treaty of Waitangi Partnership Obligation (Principles Doctrine)
 *   domain: constitutional/indigenous rights/post-colonial governance
 *
 * SUMMARY:
 *   The Treaty of Waitangi has existed since 1840 as two texts that do not
 *   agree on what was transferred; from 1975 the partnership reading was
 *   institutionalized into operative public law — the Waitangi Tribunal
 *   (1975, retroactive jurisdiction 1985), the principles doctrine
 *   articulated by the Court of Appeal in the 1987 Lands Case, statutory
 *   principles clauses, and the settlement process that has transferred
 *   roughly NZ$2.7 billion and real co-governance authority to iwi entities.
 *   This story assesses the standing arrangement that reading produces: Crown
 *   governance bounded by duties of good faith, consultation, and active
 *   protection, with settlements as redress and parliamentary supremacy
 *   intact above the doctrine. The claim/metric gap is deliberate and the two
 *   are authored independently: the arrangement is CLAIMED as tangled_rope
 *   from this reading's seat — genuine partnership coordination carrying
 *   asymmetric Crown capture — while each metric is authored from its own
 *   descriptive evidence; where the computed classification diverges from the
 *   claim, that divergence is the measurement. ε's referent is the standing
 *   arrangement under contest as this reading sees it — never the arrangement
 *   any sibling reading would endorse. The colloquial label 'the Treaty
 *   partnership' also covers structurally distinct sibling constraints (see
 *   network.dual_formulation_note): the partnership obligation itself (this
 *   story) and the full-and-final settlement machinery it legitimates.
 *
 * KEY AGENTS:
 *   - the_crown_executive: agenda-setter and principal gain-recipient (institutional / constrained exit) — administers the principles framework, drafts settlements, retains legislative override
 *   - maori_iwi_collective: primary beneficiary with real cost-bearing (organized / identity_locked) — receives settlements, consultation rights, co-governance; bears participation burden and full-and-final extinguishment
 *   - nz_taxpayers: payer (moderate / constrained) — funds settlements and Tribunal operations through general taxation
 *   - third_party_resource_users: payer (powerful / mobile) — commercial access and quota transferred or conditioned by settlements and co-governance
 *   - waitangi_tribunal: interpretive administrator (institutional / analytical) — produces the findings settlements rest on; recommendations bind only narrowly
 *   - urban_pan_tribal_maori: excluded cost-bearer (moderate / identity_locked) — outside mandated iwi structures; bears extinguishment without asset transfers
 *   - future_claimant_generations: excluded and bound (powerless / trapped) — bound by full-and-final deeds they had no part in negotiating
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__partnership_reading, 0.6).
domain_priors:suppression_score(waitangi_sovereignty_allocation__partnership_reading, 0.6).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__partnership_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__partnership_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__partnership_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__partnership_reading, "Treaty of Waitangi Partnership Obligation (Principles Doctrine)").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__partnership_reading, "constitutional/indigenous rights/post-colonial governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__partnership_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__partnership_reading, '3fe538f3-c705-48b4-8aba-41e40b17376e').
narrative_ontology:cs_kernel_codification('3fe538f3-c705-48b4-8aba-41e40b17376e', fixed_text).
narrative_ontology:cs_authority_grounding('3fe538f3-c705-48b4-8aba-41e40b17376e', lineage).
narrative_ontology:cs_interpretation_layer_present('3fe538f3-c705-48b4-8aba-41e40b17376e').
narrative_ontology:cs_reading_relation('3fe538f3-c705-48b4-8aba-41e40b17376e', waitangi_sovereignty_allocation__crown_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('3fe538f3-c705-48b4-8aba-41e40b17376e', waitangi_sovereignty_allocation__rangatiratanga_reading, influences).
narrative_ontology:cs_axiom('3fe538f3-c705-48b4-8aba-41e40b17376e', foundational, crown_maori_ongoing_partnership).
narrative_ontology:cs_axiom_status(crown_maori_ongoing_partnership, holdable).
narrative_ontology:cs_axiom_grounding('3fe538f3-c705-48b4-8aba-41e40b17376e', crown_maori_ongoing_partnership, conventional).
narrative_ontology:cs_axiom('3fe538f3-c705-48b4-8aba-41e40b17376e', foundational, active_protection_of_maori_interests).
narrative_ontology:cs_axiom_status(active_protection_of_maori_interests, holdable).
narrative_ontology:cs_axiom_grounding('3fe538f3-c705-48b4-8aba-41e40b17376e', active_protection_of_maori_interests, deontological).
narrative_ontology:cs_reference_frame('3fe538f3-c705-48b4-8aba-41e40b17376e', living_good_faith_partnership).
narrative_ontology:cs_drift_state('3fe538f3-c705-48b4-8aba-41e40b17376e', contemporary_coalition_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('3fe538f3-c705-48b4-8aba-41e40b17376e', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__partnership_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, maori_iwi_collective).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, nz_taxpayers).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, third_party_resource_users).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, urban_pan_tribal_maori).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, future_claimant_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__partnership_reading, the_crown_executive).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__partnership_reading, maori_iwi_collective).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__partnership_reading, treaty_principles_doctrine).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__partnership_reading, good_faith_partnership_doctrine).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__partnership_reading, active_protection_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and administers the principles framework: decides when and how consultation occurs, negotiates and drafts settlement deeds, published its own list of Treaty principles by Cabinet circular, and retains the legislative ability to override the framework outright, as with the 2004 foreshore and seabed legislation. Gains domestic legitimacy and international standing from operating as a Treaty partner, and gains legal closure when settlements are signed; the fiscal cost of settlements is carried through the public accounts rather than borne by the institution itself.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, the_crown_executive, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__partnership_reading, the_crown_executive, beneficiary).

% Iwi and hapū and their post-settlement governance entities receive negotiated asset transfers (Waikato-Tainui 1995, Ngāi Tahu 1998, Tūhoe 2014), co-governance appointments over rivers and conservation land, and formal consultation rights on matters affecting their taonga. Participation demands sustained organizational capacity — preparing claims, litigating, submitting to consultation — with outcomes that rarely depart from the Crown's preferred position. Settlement deeds are full and final: historical claims are extinguished on signing, and representation is channeled through mandated iwi organizations. Leaving the relationship is not a live option; the Treaty relationship is constitutive of the polity's identity and standing.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, maori_iwi_collective, beneficiary,
    organized, civilizational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__partnership_reading, maori_iwi_collective, payer).

% Fund settlement packages, Tribunal operations, and co-governance arrangements through general taxation, with limited formal voice in how settlements are structured. The annual cost is small relative to the budget, but cumulative transfers and permanent co-governance operating costs are now a fixed feature of public finance. Individual exit means emigration.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, nz_taxpayers, payer,
    moderate, biographical, constrained, national).

% Commercial fishers, energy companies, developers, and landholders whose access or title is affected by settlement transfers (for example, fisheries quota transferred under the Sealord deal), co-governance of rivers and conservation land, and consultation requirements on consents. They can relocate investment or challenge process in court, but the underlying transfers stand once settled.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, third_party_resource_users, payer,
    powerful, biographical, mobile, national).

% A standing commission of inquiry that hears historical and contemporary claims, produces the findings most settlements rest on, and has shaped the principles doctrine through its reports. Its recommendations bind the Crown only over narrow classes of land; the Crown decides whether to act on the rest. Its continued existence, staffing, and docket depend on the claims process it administers.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, waitangi_tribunal, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__partnership_reading, waitangi_tribunal, observer).

% Māori whose whakapapa ties were severed or thinned by urbanization and who are not enrolled in a mandated iwi organization. Settlements were negotiated iwi by iwi, so asset transfers and co-governance seats concentrate in iwi entities; pan-tribal urban authorities received little. They carry the collective effects of extinguished claims and of decisions made without them, and had no seat in the settlement conversations that fixed the current arrangements.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, urban_pan_tribal_maori, excluded,
    moderate, generational, identity_locked, national).

% People not yet born or not of age when settlement deeds were signed, bound by their full-and-final provisions. They had no consent mechanism; their inheritance of the relationship with the Crown was fixed by agreements their predecessors negotiated under the Crown's preferred process and timelines.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__partnership_reading, future_claimant_generations, excluded,
    powerless, generational, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__partnership_reading, the_crown_executive).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__partnership_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Structures the coexistence of two polities in one territory after a contested and textually ambiguous cession: channels disputes over land, water, and authority into inquiry, negotiation, and litigation rather than extra-legal conflict, and maintains a shared — if asymmetric — framework for co-governance of rivers, conservation land, and settlement assets.
% TRANSFER_FUNCTION: Moves settlement assets and co-governance authority from the Crown to iwi entities; moves consultation obligations and discretion limits onto the Crown; moves participation costs, representation burdens, and full-and-final claim extinguishment onto Māori; moves settlement funding onto taxpayers; and accrues legitimacy, closure, and retained governance supremacy to the Crown.
% ABSENT_VOICES: Urban pan-tribal Māori outside mandated iwi structures would object that settlements concentrated assets in iwi corporations while their claims went unanswered; future generations bound by full-and-final deeds had no seat when extinguishment was negotiated; third-party resource interests entered only through the Crown's negotiating position rather than directly. All three stand outside the settlement table where the arrangement's terms were fixed.
% DISAPPEARANCE_RATIONALE: If the partnership framework vanished overnight — the Tribunal gone, principles clauses inoperative, settlements deniable — Crown governance would revert to unconstrained parliamentary practice, iwi would lose the legal protection of settlement assets and their co-governance seats, and the channel that has contained land and water disputes since 1975 would close: the post-1975 settlement of Crown–Māori relations would unravel and the underlying contest would return to the streets and the courts without doctrine.
% FOUNDING_PROBLEM: The 1840 cession was recorded in two languages that do not agree on what was transferred, and the settler state needed a working answer to what Māori ceded and retained; by the 1970s, accumulating grievances over land confiscation, Crown purchasing, and resource taking had made the question unavoidable for the legitimacy of New Zealand governance.
% FOUNDING_PROBLEM_CORROBORATION: Waitangi Tribunal reports — from an institution the Crown funds but does not control — repeatedly find the founding question unresolved in specific domains; the independent Matike Mai Aotearoa working group (2016) concluded the constitutional question remains open and proposed transformation; constitutional scholarship outside the settlement process corroborates; and the Crown itself concedes partiality by continuing to negotiate settlements and by supporting a UN Declaration implementation plan. No party attests the problem is closed except through the settlements' own full-and-final clauses — which are the arrangement's product, not independent attestation.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__partnership_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__partnership_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__partnership_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__partnership_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__partnership_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is 0.60 at interval end: the arrangement delivers real redress and real consultation duties, but the Crown captures the surplus — legitimacy from operating as a Treaty partner, closure through full-and-final extinguishment, settlements negotiated and drafted under Crown process on the Crown's timelines, and governance supremacy retained intact above the doctrine. Suppression is 0.60 and structural rather than coercive in the ordinary sense: the ever-available parliamentary override (demonstrated in 2004), non-binding Tribunal recommendations, full-and-final clauses that bar re-litigation, and participation channels that absorb claim-making without transferring decision power. Suppression is authored as a raw structural property — the engine, not this story, scales extractiveness by directionality and scope. Theater is 0.45: consultation processes are partly performative (frequently run after operative decisions; the 2004 foreshore hui were widely reported as predetermined), while settlements and co-governance arrangements (Te Urewera, Whanganui River) are substantive transfers. Accessibility collapse is 0.45: alternatives — litigation outside the framework, political movements, constitutional transformation proposals — remain visible but are channeled and costly. Resistance is 0.65: sustained across the interval — Tribunal findings the Crown declines to implement, the Ihumātao occupation (2019), the 2024 hīkoi against the Treaty Principles Bill, and continuing litigation. The measurement series run on one shared grid (1975–2025) with all three metrics authored at every point; the 2004 suppression spike and 2014 partial relief reflect the foreshore override and its partial replacement by negotiated co-governance — a spike-and-partial-recovery, not a cycle. Coordination type is identity_coordination: the framework's primary function is maintaining the Crown–Māori boundary and membership structure (who speaks for iwi, which authority governs which sphere); its failure mode is not allocation breakdown but collapse of the shared framework itself.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the Crown's position the arrangement is a good-faith partnership it funds, honors in consultation, and extends through settlements. From the iwi seat the same structure is a partial partnership: real gains under a hard ceiling, where consultation rarely moves the operative decision and settlements close claims this reading treats as still open. From the taxpayer seat it is a standing cost without voice. From the excluded seats it is a conversation that produced binding terms in their absence. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   maori_iwi_collective is declared beneficiary and derives low directionality — it receives the settlements and rights — but identity_locked exit and its secondary payer position (participation costs, extinguishment) keep it off the pure beneficiary end. nz_taxpayers and third_party_resource_users are declared victims and derive high directionality; the commercial seat's mobile exit damps its effective extraction relative to the taxpayer seat's constrained position. The excluded seats derive high directionality from their victim declarations. One override is declared: institutional seats at d=0.35. The array-based derivation cannot see that the Crown captures the arrangement's legitimacy surplus while externalizing settlement costs to taxpayers — its net position sits below symmetric despite administering the arrangement — and the Tribunal's institutional existence is sustained by the claims process it administers. Both institutional seats sit genuinely below symmetric, so a single per-atom override approximates both.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — what the 1840 cession transferred and where authority therefore sits — is live, not dead: settlements resolve specific historical claims while the constitutional question remains open, and the 2020s repudiation pressure shows the mandate is contested rather than atrophied. The tangled_rope classification prevents both characteristic mislabels: reading the arrangement as pure extraction would erase the genuine coordination it performs — real transfers, real co-governance, a channel that has contained dispute for five decades; reading it as pure coordination would erase the asymmetry — Crown capture of legitimacy, discount-priced extinguishment, a ceiling the Crown can legislate through. No mandatrophy declaration is made: the mandate has not outlived its function; it is under-delivered and contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_structural_delta,
    'This constraint is one reading (partnership_reading) of the waitangi_sovereignty_allocation kernel. What structural facts — victim sets, ε, enforcement profile — change under the sibling readings crown_sovereignty_reading and rangatiratanga_reading?',
    'Authoring and classifying the sibling stories in their own files: crown_sovereignty_reading (no partnership duties; the arrangement collapses into ordinary parliamentary practice with negligible measured extraction) and rangatiratanga_reading (Crown governance over Māori lands and taonga without full consent becomes the extractive surface, with the victim set expanding to Māori as a whole).',
    'Under the crown sovereignty sibling, this arrangement''s coordination duties vanish and extraction collapses toward an ordinary-government baseline; under the rangatiratanga sibling, extraction rises sharply and the payer set changes composition entirely. Cross-reading comparisons are invalid unless each reading is classified separately — the ε values are reading-indexed over the same referent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer structure: which kernel this is, which reading this instantiates, and what the sibling readings would change structurally.').

omega_variable(
    bilingual_text_divergence_location,
    'The kernel''s textual ambiguity has two loci: does Article I record a cession of ''sovereignty'' (English text) or a grant of ''kāwanatanga'' (Māori text), and does Article II''s guarantee of ''taonga'' extend to intangibles (language, knowledge, authority) or only tangible treasures? Which textual element does the disagreement between readings actually turn on?',
    'Historical-linguistic analysis of 1840 signatory understanding (He Whakaputanga context, missionary translation records) combined with doctrinal analysis of which reading each textual element supports.',
    'If the disagreement turns on Article I''s cession verb, the readings are mutually exclusive claims about what was transferred and sibling relations skew toward foreclosure-shaped structure; if it turns on taonga''s scope, the readings can partially overlap (shared authority over an expanding protected sphere) and relations skew toward competition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bilingual_text_divergence_location, conceptual, 'Where in the divergent Treaty texts the kernel disagreement is located.').

omega_variable(
    settlement_quantum_adequacy,
    'Are settlement packages proportionate redress for the losses they compensate, or discount-priced extinguishment of claims whose full value exceeds the quantum by orders of magnitude?',
    'Economic valuation of confiscated and Crown-purchased lands and resources at current value versus settlement quantum; Waitangi Tribunal findings on loss; comparison of cumulative settlement totals (roughly NZ$2.7 billion over three decades) against estimated historical loss value.',
    'Deep-discount findings raise the arrangement''s measured extraction and confirm the Crown as the seat the gains accrue to; proportionate findings move the arrangement toward a coordination reading in which settlements are ordinary transfers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settlement_quantum_adequacy, empirical, 'Whether redress quantum tracks loss value or purchases cheap closure.').

omega_variable(
    full_and_final_intergenerational_binding,
    'Do full-and-final settlement clauses legitimately bind future generations of claimants, or do they manufacture consent by extinguishing the claims of people who never agreed?',
    'Comparative analysis of intergenerational consent in indigenous settlements internationally; monitoring of post-settlement claim re-emergence (for example, contemporary freshwater claims arising after historical settlements were signed).',
    'If full-and-final clauses are illegitimate intergenerationally, the arrangement''s suppression component rises (it suppresses future claims, not only present ones) and the excluded-seat analysis extends to all future Māori; if legitimate, extinguishment is ordinary finality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(full_and_final_intergenerational_binding, conceptual, 'Legitimacy of extinguishing future generations'' claims by present agreement.').

omega_variable(
    consultation_substance_ambiguity,
    'Is the consultation the Crown conducts substantive input capable of changing decisions, or predominantly performative process run after the operative decision has been made?',
    'Tracing consultation records against decision points across a sample of Crown decisions affecting Māori interests — did submissions alter outcomes? — combined with judicial review outcomes on the adequacy of consultation.',
    'Predominantly performative consultation raises theater_ratio and suppression (participation without effect), pushing the arrangement toward the extraction end; substantively effective consultation supports the coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consultation_substance_ambiguity, empirical, 'Whether consultation input is substantive or performative.').

omega_variable(
    principles_doctrine_ceiling,
    'Does the principles doctrine bind only the executive (through statutory principles clauses and judicial review), or does it reach Parliament — and if only the executive, is the arrangement''s ceiling a Crown choice revocable at will?',
    'Constitutional analysis of parliamentary supremacy versus any entrenched Treaty obligation; the outcome of the Treaty Principles Bill process; whether any entrenchment mechanism is ever adopted.',
    'Executive-only binding means the arrangement persists at the Crown''s discretion and its protection is revocable — persistence is a political fact rather than a legal one; parliamentary reach would convert the ceiling into a structural limit and materially lower the Crown''s effective exit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(principles_doctrine_ceiling, conceptual, 'The constitutional ceiling of the partnership obligation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__partnership_reading, 1975, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wait_tr_t1975, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 1975, 0.2).
narrative_ontology:measurement_basis(wait_tr_t1975, observed).
narrative_ontology:measurement(wait_tr_t1985, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 1985, 0.22).
narrative_ontology:measurement_basis(wait_tr_t1985, observed).
narrative_ontology:measurement(wait_tr_t1987, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 1987, 0.25).
narrative_ontology:measurement_basis(wait_tr_t1987, observed).
narrative_ontology:measurement(wait_tr_t1995, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 1995, 0.33).
narrative_ontology:measurement_basis(wait_tr_t1995, observed).
narrative_ontology:measurement(wait_tr_t2004, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 2004, 0.45).
narrative_ontology:measurement_basis(wait_tr_t2004, observed).
narrative_ontology:measurement(wait_tr_t2014, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 2014, 0.4).
narrative_ontology:measurement_basis(wait_tr_t2014, observed).
narrative_ontology:measurement(wait_tr_t2025, waitangi_sovereignty_allocation__partnership_reading, theater_ratio, 2025, 0.45).
narrative_ontology:measurement_basis(wait_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(wait_be_t1975, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 1975, 0.28).
narrative_ontology:measurement_basis(wait_be_t1975, observed).
narrative_ontology:measurement(wait_be_t1985, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 1985, 0.33).
narrative_ontology:measurement_basis(wait_be_t1985, observed).
narrative_ontology:measurement(wait_be_t1987, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 1987, 0.38).
narrative_ontology:measurement_basis(wait_be_t1987, observed).
narrative_ontology:measurement(wait_be_t1995, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 1995, 0.47).
narrative_ontology:measurement_basis(wait_be_t1995, observed).
narrative_ontology:measurement(wait_be_t2004, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 2004, 0.54).
narrative_ontology:measurement_basis(wait_be_t2004, observed).
narrative_ontology:measurement(wait_be_t2014, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 2014, 0.57).
narrative_ontology:measurement_basis(wait_be_t2014, observed).
narrative_ontology:measurement(wait_be_t2025, waitangi_sovereignty_allocation__partnership_reading, base_extractiveness, 2025, 0.6).
narrative_ontology:measurement_basis(wait_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(wait_su_t1975, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 1975, 0.25).
narrative_ontology:measurement_basis(wait_su_t1975, observed).
narrative_ontology:measurement(wait_su_t1985, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 1985, 0.32).
narrative_ontology:measurement_basis(wait_su_t1985, observed).
narrative_ontology:measurement(wait_su_t1987, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 1987, 0.38).
narrative_ontology:measurement_basis(wait_su_t1987, observed).
narrative_ontology:measurement(wait_su_t1995, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 1995, 0.42).
narrative_ontology:measurement_basis(wait_su_t1995, observed).
narrative_ontology:measurement(wait_su_t2004, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 2004, 0.56).
narrative_ontology:measurement_basis(wait_su_t2004, observed).
narrative_ontology:measurement(wait_su_t2014, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 2014, 0.46).
narrative_ontology:measurement_basis(wait_su_t2014, observed).
narrative_ontology:measurement(wait_su_t2025, waitangi_sovereignty_allocation__partnership_reading, suppression_requirement, 2025, 0.6).
narrative_ontology:measurement_basis(wait_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__partnership_reading, identity_coordination).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, treaty_settlement_full_final_machinery).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__partnership_reading, foreshore_seabed_legislative_override).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Treaty partnership' covers at least two structurally distinct constraints with different ε: (1) the partnership obligation itself — the principles doctrine binding Crown conduct (this story, upstream, higher-confidence legal frame); (2) the settlement machinery — the full-and-final, Crown-drafted extinguishment process (treaty_settlement_full_final_machinery, downstream) — where the partnership frame's under-delivery is cashed out as discount-priced closure. The 2004 foreshore and seabed override (foreshore_seabed_legislative_override) is recorded as a second downstream edge: it demonstrates the parliamentary ceiling the principles doctrine cannot breach. Per the ε-invariance principle these are separate stories with separate stakeholders, linked here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(waitangi_sovereignty_allocation__partnership_reading, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
