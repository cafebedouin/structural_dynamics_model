% ============================================================================
% CONSTRAINT STORY: waitangi_sovereignty_allocation__crown_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_waitangi_sovereignty_allocation__crown_sovereignty_reading, []).

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
 *   constraint_id: waitangi_sovereignty_allocation__crown_sovereignty_reading
 *   human_readable: Crown Sovereignty Reading of Te Tiriti Article I (Plenary Cession)
 *   domain: constitutional_law/indigenous_rights/post_colonial_governance
 *
 * SUMMARY:
 *   The English-language draft presented to rangatira in 1840 states that the
 *   chiefs cede to the Queen 'all the rights and powers of sovereignty' over
 *   their territories. The crown_sovereignty_reading takes that sentence as
 *   complete and effective: sovereignty passed wholly to the Crown,
 *   Westminster-style parliamentary supremacy governs New Zealand, and
 *   Parliament may legislate for Māori interests without their consent. On
 *   this reading the arrangement is the lawful foundation of the New Zealand
 *   state — and simultaneously the instrument through which the Māori
 *   land-base moved: Crown pre-emption of purchase, the Native Land Court's
 *   individualization of customary title, the confiscations of the 1860s, and
 *   a century of statutes enacted over Māori objection all proceed under the
 *   authority Article I is read to confer. This file instantiates ONE reading
 *   of the waitangi_sovereignty_allocation kernel; the partnership and
 *   rangatiratanga readings are separate constraint stories with their own
 *   epsilon values, victim structures, and classifications, linked through
 *   network.affects_constraints. KEY AGENTS (by structural relationship): -
 *   the_crown: Primary agenda-setter and beneficiary
 *   (institutional/arbitrage) — holds the asserted plenary authority, writes
 *   and amends the rules - settler_colonists: Primary material beneficiary
 *   (organized/mobile) — received land through Crown machinery; exit by
 *   migration remains possible - maori_iwi_hapu: Primary target/payer
 *   (powerless/identity_locked) — bore confiscation, title individualization,
 *   and legislative subordination - rangatira_signatories: Payer and excluded
 *   voice (moderate/trapped) — assented on the Māori text's terms, never
 *   seated in the resulting order - native_land_court: Administrative
 *   agenda-setter (institutional/constrained) — ran the alienation machinery,
 *   funded by its own throughput - waitangi_tribunal: Analytical observer
 *   (institutional/analytical) — sees the whole structure, recommends without
 *   binding
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.52).
domain_priors:suppression_score(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.3).
domain_priors:theater_ratio(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(waitangi_sovereignty_allocation__crown_sovereignty_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(waitangi_sovereignty_allocation__crown_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(waitangi_sovereignty_allocation__crown_sovereignty_reading, "Crown Sovereignty Reading of Te Tiriti Article I (Plenary Cession)").
narrative_ontology:topic_domain(waitangi_sovereignty_allocation__crown_sovereignty_reading, "constitutional_law/indigenous_rights/post_colonial_governance").

domain_priors:requires_active_enforcement(waitangi_sovereignty_allocation__crown_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(waitangi_sovereignty_allocation__crown_sovereignty_reading, 'fb350a46-0468-41ec-b111-416fea3c2765').
narrative_ontology:cs_kernel_codification('fb350a46-0468-41ec-b111-416fea3c2765', fixed_text).
narrative_ontology:cs_authority_grounding('fb350a46-0468-41ec-b111-416fea3c2765', lineage).
narrative_ontology:cs_interpretation_layer_present('fb350a46-0468-41ec-b111-416fea3c2765').
narrative_ontology:cs_reading_relation('fb350a46-0468-41ec-b111-416fea3c2765', waitangi_sovereignty_allocation__partnership_reading, coexists_with).
narrative_ontology:cs_reading_relation('fb350a46-0468-41ec-b111-416fea3c2765', waitangi_sovereignty_allocation__rangatiratanga_reading, forecloses).
narrative_ontology:cs_axiom('fb350a46-0468-41ec-b111-416fea3c2765', foundational, article_one_effected_complete_cession).
narrative_ontology:cs_axiom_status(article_one_effected_complete_cession, holdable).
narrative_ontology:cs_axiom_grounding('fb350a46-0468-41ec-b111-416fea3c2765', article_one_effected_complete_cession, empirically_contingent).
narrative_ontology:cs_axiom('fb350a46-0468-41ec-b111-416fea3c2765', secondary, parliament_may_legislate_over_maori_without_consent).
narrative_ontology:cs_axiom_status(parliament_may_legislate_over_maori_without_consent, holdable).
narrative_ontology:cs_axiom_grounding('fb350a46-0468-41ec-b111-416fea3c2765', parliament_may_legislate_over_maori_without_consent, conventional).
narrative_ontology:cs_reference_frame('fb350a46-0468-41ec-b111-416fea3c2765', ceded_plenary_sovereignty_westminster_supremacy).
narrative_ontology:cs_drift_state('fb350a46-0468-41ec-b111-416fea3c2765', contemporary_treaty_principles_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('fb350a46-0468-41ec-b111-416fea3c2765', '').
narrative_ontology:cs_kernel_id(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, the_crown).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, settler_colonists).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_iwi_hapu).
narrative_ontology:constraint_victim(waitangi_sovereignty_allocation__crown_sovereignty_reading, rangatira_signatories).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(waitangi_sovereignty_allocation__crown_sovereignty_reading, native_land_court).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__crown_sovereignty_reading, westminster_parliamentary_supremacy).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__crown_sovereignty_reading, doctrine_of_cession).
narrative_ontology:constraint_vindicates(waitangi_sovereignty_allocation__crown_sovereignty_reading, act_of_state_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the legislative authority this reading attributes to it: drafts, enacts, and amends the statutes that define land tenure, confiscation, and settlement. Collects allegiance, customs revenue, and the capacity to dispose of land it deems acquired. Because it writes the rules it operates under, it can restructure its own commitments, though diplomatic reputation and relational stability with iwi raise the price of open unilateralism.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, the_crown, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__crown_sovereignty_reading, the_crown, beneficiary).

% Received the great bulk of land transferred through Crown purchase, pre-emption waiver, and grant after confiscation; gained political representation, courts, roads, and schools financed in part from that land. Migration brought them here voluntarily and remained open, so their presence is choosable in a way that residence on ancestral land is not.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, settler_colonists, beneficiary,
    organized, generational, mobile, regional).

% Hold whakapapa ties to specific lands, rivers, and taonga that cannot be relocated without severing identity. Across the interval they lost the great majority of their land-base through purchase, individualization of title, and confiscation; they now operate inside statutes enacted without their consent, pursuing recognition through petitions, litigation, the Waitangi Tribunal, and settlements negotiated on Crown-defined parameters.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, maori_iwi_hapu, payer,
    powerless, civilizational, identity_locked, national).

% The chiefs who marked the sheets in 1840 after debate conducted largely in te reo on the Māori text. Their stated understanding — a governor for the settlers, chiefly authority retained — was never embodied in the institutions that followed; later parliaments defined the Treaty's effect without seating them or their successors as constitutional parties.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, rangatira_signatories, payer,
    moderate, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__crown_sovereignty_reading, rangatira_signatories, excluded).

% The statutory court created in 1865 to convert customary tenure into individual, alienable title. Its judges and registrars were paid from the business it processed, and its sittings determined which blocks could be sold and to whom. It administered the conversion block by block; it could accelerate or delay particular cases but not question the mandate.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, native_land_court, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(waitangi_sovereignty_allocation__crown_sovereignty_reading, native_land_court, beneficiary).

% A permanent commission of inquiry established in 1975 with retrospective jurisdiction from 1985. It receives claims, researches the historical record across both Treaty texts, and reports on Crown conduct measured against Treaty principles. Its recommendations do not bind the Crown; settlements proceed through negotiation the Crown controls.
narrative_ontology:constraint_stakeholder(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_tribunal, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(waitangi_sovereignty_allocation__crown_sovereignty_reading, settler_colonists).
narrative_ontology:fixing_cost_class(waitangi_sovereignty_allocation__crown_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a single sovereign legal authority spanning settler settlements and iwi territories: one law for contracts, crime, and land transactions where previously iwi authority, settler self-help, and consular jurisdiction overlapped without arbitration.
% TRANSFER_FUNCTION: Moved legislative authority, land, and resource-control decisions from hapū and iwi collectives to the Crown, and onward through Crown purchase and grant to individual settlers; moved the costs of administration onto Māori landholdings through court fees, survey charges, and rates.
% ABSENT_VOICES: Rangatira who refused to sign (some Waikato, Tūhoe, and South Island leaders), signatories whose understanding followed the Māori text, and the unborn generations who inherit the allocation without having consented. They are absent from the drafting record — the English text was finalized without Māori participation — and from the parliamentary forums that later defined the Treaty's legal effect.
% DISAPPEARANCE_RATIONALE: If the Article I allocation vanished overnight, every property title traceable to Crown grant would rest on a broken chain, the New Zealand state's claim to exclusive lawmaking authority would lapse, and iwi assertions of continuing authority would move from claim to operative fact; the constitutional order would require wholesale renegotiation rather than administrative patching.
% FOUNDING_PROBLEM: In 1840: settler settlements beyond effective law (Kororāreka's disorder), unregulated and often fraudulent land dealing, imperial concern over rival colonial ambitions, and Māori interest in regulated trade, firearms control, and protection from lawless settlers.
% FOUNDING_PROBLEM_CORROBORATION: Imperial dispatches (Normanby's 1839 instructions to Hobson), missionary correspondence (Williams, Maunsell), and rangatira speeches recorded at Kohimarama in 1860 attest the governance-and-order problem from seats outside the settler-benefit position; historians (Orange, Fletcher, Belich) corroborate that the problem was real while disputing whether solving it required plenary cession. No source outside the benefiting parties attests that the problem required the unilateral form this reading asserts.
narrative_ontology:disappearance_verdict(waitangi_sovereignty_allocation__crown_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(waitangi_sovereignty_allocation__crown_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(waitangi_sovereignty_allocation__crown_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(waitangi_sovereignty_allocation__crown_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(waitangi_sovereignty_allocation__crown_sovereignty_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(waitangi_sovereignty_allocation__crown_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(waitangi_sovereignty_allocation__crown_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(waitangi_sovereignty_allocation__crown_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52 at interval end) is authored from the arrangement's transfer record: this reading's own lights treat the cession as lawful, but the descriptive series records what moved — by 1930 iwi retained under a tenth of their 1840 land-base, and the machinery that moved it (pre-emption, the Native Land Court, raupatu) operated under statutes this reading validates. The series falls after 1985 because retroactive Tribunal jurisdiction, the State-Owned Enterprises principles clause, and the fisheries and historical settlements returned a small fraction of value and imposed consultation costs on the Crown. Suppression (0.30) traces enforcement intensity: built to a wartime peak across the 1860s–70s, sustained through court-and-police administration into the 1930s, then decaying as compliance normalized and resistance migrated into litigation and inquiry. Theater (0.48) rises monotonically: ceremonial and rhetorical recognition (Waitangi Day observance, principles recitals, partnership vocabulary) grows fastest precisely where structural revision is smallest. Accessibility collapse (0.58) is partial: within this reading's framework the rival-sovereignty alternative is doctrinally foreclosed (the Wi Parata nullity line, parliamentary supremacy), yet the alternative stays organizationally alive — Kingitanga continuity, Kotahitanga memory, Matike Mai's constitutional transformation work — so alternatives narrow without vanishing. Resistance (0.70) is high and sustained: armed conflict, mass petitioning, parallel parliaments, decades of litigation, and repeated land marches; the arrangement has never operated uncontested. Claim/metric independence: claimed_type is tangled_rope because the structure genuinely solved a coordination problem (a single enforceable legal order across overlapping jurisdictions) while transferring value asymmetrically under active enforcement; the engine computes per-seat types from the structural data and may disagree with the claim.
 *
 * PERSPECTIVAL GAP:
 *   From the Crown seat the arrangement is the founding act of government: coercion in the 1860s reads as enforcement of law the chiefs had joined, and modern consultation reads as generosity beyond obligation. From the identity_locked payer seat the identical statutes read as expropriation carried out under a text whose Māori meaning was never adopted. The same document functions as a title deed in one seat and as a fraud allegation in another; the engine computes this divergence from power, exit, and role data rather than resolving it.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown sits near the beneficiary pole: it collects the authority and controls amendment, so its derived d lands low despite bearing enforcement costs. Settler colonists sit nearest zero: they received the material transfer and hold mobility the other seats lack, which pushes their effective position toward subsidy. Māori iwi and hapū sit near the target pole — identity_locked exit amplifies effective extraction because the asset taken (ancestral land and waters bound to whakapapa) cannot be repurchased on equivalent terms anywhere else. The Native Land Court is institutionally positioned mid-to-high: it administered rather than designed the allocation, but its funding rode the throughput it processed. The Waitangi Tribunal is analytical and near-symmetric by construction: it observes and recommends without collecting or paying.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — lawful order across overlapping jurisdictions — remains live in the minimal sense that any polity needs settled authority, but the contested element is the form: plenary power exercisable without Māori consent. Authoring founding_problem_status as contested rather than dead keeps the mismatch consumer from flagging a zombie (dead problem plus world_rearranges) while honestly recording that the parties dispute whether the original justification still covers the current arrangement. The tangled_rope claim is what prevents misclassification in both directions: reading the structure as pure snare erases the real coordination delivered (one enforceable legal order, regulated trade, protection some rangatira sought from lawless settlers); reading it as pure rope erases raupatu, the court's conversion machinery, and a century of unilateral legislation. The mandatrophy risk here runs forward: as settlements and principles rhetoric accumulate, the arrangement can decay into performance maintained by anniversary ceremony — the rising theater series is the early warning.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the waitangi_sovereignty_allocation kernel — does classifying it in isolation understate the contest, given that partnership_reading and rangatiratanga_reading instantiate different constraints from the same 1840 instrument?',
    'Cross-file comparison of the three reading-stories: divergence in epsilon, victim sets, and computed types across readings is the measurement of the kernel contest; convergence would suggest the readings collapse into one.',
    'If the sibling readings classify materially differently, the kernel is genuinely contested and no single-file verdict about ''the Treaty'' is meaningful; if they converge, the contest is rhetorical rather than structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer-frame routing: this story is one reading of a three-reading kernel, not the kernel itself.').

omega_variable(
    translation_equivalence_of_cession,
    'Did the English text''s ''all the rights and powers of sovereignty'' and the Māori text''s ''kāwanatanga'' convey the same grant to the 1840 signatories?',
    'Philological reconstruction of 1840 usage (missionary translation practice, analyses of the surviving English drafts), comparison of the printed Māori text against the English drafts, and records of what was explained to signatories at each sheet.',
    'Material divergence undermines the cession premise beneath this reading''s foundational axiom, shifting drift toward repudiation_pressure and strengthening the rangatiratanga reading''s position; textual equivalence consolidates this reading''s reference frame.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(translation_equivalence_of_cession, empirical, 'Whether the two Treaty texts state one grant or two different ones.').

omega_variable(
    rangatira_intent_cession_vs_governorship,
    'Did the rangatira who signed understand themselves as transferring full sovereignty or as granting a governorship over settlers while retaining their own authority?',
    'Contemporary speech records (Kohimarama 1860 and earlier hui), oral tradition documented under iwi control, and historian synthesis weighing both bodies of evidence.',
    'Consent-to-cession sustains this reading''s legitimacy independently of textual equivalence; consent-to-governorship converts the arrangement into construction without consent, feeding the foreclosure computation against rangatiratanga_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rangatira_intent_cession_vs_governorship, empirical, 'Historical intent of the signatory parties regarding what was given up.').

omega_variable(
    persistence_basis_inertia_vs_consent,
    'Does the arrangement persist today because it commands acceptance, or because the cost of constitutional replacement exceeds any seat''s willingness to pay?',
    'Track uptake of constitutional transformation proposals (Matike Mai and successors), iwi mandate processes, and revealed willingness to bear transition costs at constitutional moments.',
    'Inertia-dominant persistence pushes the arrangement toward degraded-operation dynamics within this reading''s own frame; consent-dominant persistence supports stable operation of the reference frame.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(persistence_basis_inertia_vs_consent, empirical, 'Whether persistence reflects agreement or exit-cost asymmetry.').

omega_variable(
    modern_consent_of_the_governed,
    'Would iwi, consulted fresh with a real alternative on the table, allocate sovereignty on the Article I terms this reading asserts?',
    'Deliberative constitutional process with iwi mandate and a credible independence option; preference revelation rather than historical inference.',
    'Preference-class: bears on legitimacy and on the sustainability of the reading''s reference frame, not on the descriptive metric series.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(modern_consent_of_the_governed, preference, 'Counterfactual present-day consent of the governed to the allocation''s terms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(waitangi_sovereignty_allocation__crown_sovereignty_reading, 1840, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wsa_crown_reading_tr_t1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1840, 0.08).
narrative_ontology:measurement_basis(wsa_crown_reading_tr_t1840, observed).
narrative_ontology:measurement(wsa_crown_reading_tr_t1870, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1870, 0.12).
narrative_ontology:measurement_basis(wsa_crown_reading_tr_t1870, observed).
narrative_ontology:measurement(wsa_crown_reading_tr_t1900, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1900, 0.18).
narrative_ontology:measurement_basis(wsa_crown_reading_tr_t1900, observed).
narrative_ontology:measurement(wsa_crown_reading_tr_t1930, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1930, 0.22).
narrative_ontology:measurement_basis(wsa_crown_reading_tr_t1930, observed).
narrative_ontology:measurement(wsa_crown_reading_tr_t1960, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1960, 0.28).
narrative_ontology:measurement_basis(wsa_crown_reading_tr_t1960, observed).
narrative_ontology:measurement(wsa_crown_reading_tr_t1990, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 1990, 0.42).
narrative_ontology:measurement_basis(wsa_crown_reading_tr_t1990, observed).
narrative_ontology:measurement(wsa_crown_reading_tr_t2025, waitangi_sovereignty_allocation__crown_sovereignty_reading, theater_ratio, 2025, 0.48).
narrative_ontology:measurement_basis(wsa_crown_reading_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(wsa_crown_reading_be_t1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1840, 0.5).
narrative_ontology:measurement_basis(wsa_crown_reading_be_t1840, observed).
narrative_ontology:measurement(wsa_crown_reading_be_t1870, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1870, 0.8).
narrative_ontology:measurement_basis(wsa_crown_reading_be_t1870, observed).
narrative_ontology:measurement(wsa_crown_reading_be_t1900, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1900, 0.86).
narrative_ontology:measurement_basis(wsa_crown_reading_be_t1900, observed).
narrative_ontology:measurement(wsa_crown_reading_be_t1930, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1930, 0.87).
narrative_ontology:measurement_basis(wsa_crown_reading_be_t1930, observed).
narrative_ontology:measurement(wsa_crown_reading_be_t1960, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1960, 0.82).
narrative_ontology:measurement_basis(wsa_crown_reading_be_t1960, observed).
narrative_ontology:measurement(wsa_crown_reading_be_t1990, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement_basis(wsa_crown_reading_be_t1990, observed).
narrative_ontology:measurement(wsa_crown_reading_be_t2025, waitangi_sovereignty_allocation__crown_sovereignty_reading, base_extractiveness, 2025, 0.52).
narrative_ontology:measurement_basis(wsa_crown_reading_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(wsa_crown_reading_su_t1840, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1840, 0.35).
narrative_ontology:measurement_basis(wsa_crown_reading_su_t1840, observed).
narrative_ontology:measurement(wsa_crown_reading_su_t1870, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1870, 0.78).
narrative_ontology:measurement_basis(wsa_crown_reading_su_t1870, observed).
narrative_ontology:measurement(wsa_crown_reading_su_t1900, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1900, 0.72).
narrative_ontology:measurement_basis(wsa_crown_reading_su_t1900, observed).
narrative_ontology:measurement(wsa_crown_reading_su_t1930, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1930, 0.62).
narrative_ontology:measurement_basis(wsa_crown_reading_su_t1930, observed).
narrative_ontology:measurement(wsa_crown_reading_su_t1960, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1960, 0.5).
narrative_ontology:measurement_basis(wsa_crown_reading_su_t1960, observed).
narrative_ontology:measurement(wsa_crown_reading_su_t1990, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 1990, 0.38).
narrative_ontology:measurement_basis(wsa_crown_reading_su_t1990, observed).
narrative_ontology:measurement(wsa_crown_reading_su_t2025, waitangi_sovereignty_allocation__crown_sovereignty_reading, suppression_requirement, 2025, 0.3).
narrative_ontology:measurement_basis(wsa_crown_reading_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(waitangi_sovereignty_allocation__crown_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation__partnership_reading).
narrative_ontology:affects_constraint(waitangi_sovereignty_allocation__crown_sovereignty_reading, waitangi_sovereignty_allocation__rangatiratanga_reading).

% DUAL FORMULATION NOTE:
% 'The Treaty of Waitangi' names one instrument but three structurally distinct claims about what it did. This file carries the crown_sovereignty_reading alone: its epsilon is authored for the standing arrangement (Crown plenary sovereignty as exercised) assessed by this reading's own lights, and its foundational axiom (complete cession) is empirically contingent — exactly the premise the rangatiratanga reading denies and the partnership reading sidesteps. The readings are kept as separate files because measuring them jointly would make epsilon observer-dependent, violating epsilon-invariance. The crown reading is upstream historically: courts invoked cession to dismiss Māori claims (Wi Parata 1877 through the Ninety-Mile Beach line), shaping the doctrinal environment in which the sibling readings operate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
