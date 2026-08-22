% ============================================================================
% CONSTRAINT STORY: ip_category_emergence__thinkability_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ip_category_emergence__thinkability_reading, []).

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
 *   constraint_id: ip_category_emergence__thinkability_reading
 *   human_readable: Ownable Expression as Legal Category (Thinkability Reading of the 1710 Emergence)
 *   domain: legal_philosophy/intellectual_property/historical_jurisprudence
 *
 * SUMMARY:
 *   This story instantiates the thinkability reading of the
 *   ip_category_emergence kernel: 1710 marks category emergence — ownable
 *   expression became a legally coherent claim-form with the Statute of Anne.
 *   Before 1710, disputes over texts ran through guild privilege, crown
 *   licensing, and property in physical copies; the claim 'I own this
 *   expression' had no legal home. After 1710, 'copy right' was deployed as a
 *   distinct claim — author-titled, term-limited, registrable, transferable —
 *   and the conceptual space of text-claims gained a point it had lacked. The
 *   constraint under classification is that standing category itself: the
 *   arrangement that makes expression born inside an ownability frame,
 *   assessed by this reading's own lights (the ε referent is the category as
 *   it stands, never the commons arrangement this or any sibling reading
 *   might endorse). Per the one-reading discipline, the sibling readings —
 *   first_holding_reading (1710 as claimant-set entry) and
 *   synchronic_diachronic_seam (the M4/M5 collapse test of the
 *   thinkability/first-holding distinction) — are separate constraint files
 *   linked via reading_relations and network edges, not folded into this one.
 *   Claim and metrics are authored independently: the type claim is what I
 *   believe structurally true; the metrics describe the category's actual
 *   operation over 1695–1774.
 *
 * KEY AGENTS:
 *   - authors: titular holder seat (moderate/constrained) — the category names them as the source of rights; most convey their terms to the trade for lump sums
 *   - london_book_trade: operative beneficiary-administrator (organized/arbitrage) — buys assignments, runs the Hall register, litigates in Chancery, and switches among privilege, statutory-right, and natural-property framings
 *   - reading_public: payer with partial offsetting gain (powerless/constrained) — pays the access premium, receives new titles and the term-expiry commons; unseated throughout
 *   - subsequent_creators: dual payer/holder seat (moderate/constrained) — raw material arrives encumbered, outputs become assets
 *   - provincial_scottish_printers: excluded would-be copiers (organized/constrained) — the commons side of the boundary, present as defendants until 1774 vindicates their position
 *   - chancery_common_law_courts: agenda-setter (institutional/constrained) — administers the boundary; decides the category's shape at Millar and Donaldson
 *   - parliament: agenda-setter (institutional/constrained) — enacted the category and retains statutory power over it
 *   - legal_doctrinal_commentators: analytical observer (analytical/analytical) — supplies the vocabulary in which the emergence is described and disputed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ip_category_emergence__thinkability_reading, 0.6).
domain_priors:suppression_score(ip_category_emergence__thinkability_reading, 0.65).
domain_priors:theater_ratio(ip_category_emergence__thinkability_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(ip_category_emergence__thinkability_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ip_category_emergence__thinkability_reading, tangled_rope).
narrative_ontology:human_readable(ip_category_emergence__thinkability_reading, "Ownable Expression as Legal Category (Thinkability Reading of the 1710 Emergence)").
narrative_ontology:topic_domain(ip_category_emergence__thinkability_reading, "legal_philosophy/intellectual_property/historical_jurisprudence").

domain_priors:requires_active_enforcement(ip_category_emergence__thinkability_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ip_category_emergence__thinkability_reading, '2ade2ff8-96b2-4337-b8c2-c0b685d31f73').
narrative_ontology:cs_kernel_codification('2ade2ff8-96b2-4337-b8c2-c0b685d31f73', formalized).
narrative_ontology:cs_authority_grounding('2ade2ff8-96b2-4337-b8c2-c0b685d31f73', lineage).
narrative_ontology:cs_interpretation_layer_present('2ade2ff8-96b2-4337-b8c2-c0b685d31f73').
narrative_ontology:cs_reading_relation('2ade2ff8-96b2-4337-b8c2-c0b685d31f73', ip_category_emergence__first_holding_reading, coexists_with).
narrative_ontology:cs_reading_relation('2ade2ff8-96b2-4337-b8c2-c0b685d31f73', ip_category_emergence__synchronic_diachronic_seam, influences).
narrative_ontology:cs_axiom('2ade2ff8-96b2-4337-b8c2-c0b685d31f73', foundational, ownable_expression_category_emerged_1710).
narrative_ontology:cs_axiom_status(ownable_expression_category_emerged_1710, holdable).
narrative_ontology:cs_axiom_grounding('2ade2ff8-96b2-4337-b8c2-c0b685d31f73', ownable_expression_category_emerged_1710, empirically_contingent).
narrative_ontology:cs_axiom('2ade2ff8-96b2-4337-b8c2-c0b685d31f73', secondary, copy_right_distinct_from_guild_privilege).
narrative_ontology:cs_axiom_status(copy_right_distinct_from_guild_privilege, holdable).
narrative_ontology:cs_axiom_grounding('2ade2ff8-96b2-4337-b8c2-c0b685d31f73', copy_right_distinct_from_guild_privilege, empirically_contingent).
narrative_ontology:cs_reference_frame('2ade2ff8-96b2-4337-b8c2-c0b685d31f73', pre_ip_vocabulary_claim_space).
narrative_ontology:cs_drift_state('2ade2ff8-96b2-4337-b8c2-c0b685d31f73', post_donaldson_statutory_settlement, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('2ade2ff8-96b2-4337-b8c2-c0b685d31f73', '').
narrative_ontology:cs_kernel_id(ip_category_emergence__thinkability_reading, ip_category_emergence).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, authors).
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, london_book_trade).
narrative_ontology:constraint_victim(ip_category_emergence__thinkability_reading, reading_public).
narrative_ontology:constraint_victim(ip_category_emergence__thinkability_reading, subsequent_creators).
narrative_ontology:constraint_victim(ip_category_emergence__thinkability_reading, provincial_scottish_printers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, reading_public).
narrative_ontology:constraint_beneficiary(ip_category_emergence__thinkability_reading, subsequent_creators).
narrative_ontology:constraint_vindicates(ip_category_emergence__thinkability_reading, incentive_justification_for_expression_property).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Compose manuscripts and, after 1710, convey 'copy right' as a transferable claim rather than selling sheets or relying on patronage alone. The statute titles them as the holders from whom rights flow; in practice most sell their terms outright to the London trade for lump sums. Exit is not available: once a work exists in the jurisdiction it is born inside the category, and dedicating a work to the commons is itself an act performed with the category's own machinery.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, authors, beneficiary,
    moderate, biographical, constrained, national).

% The London booksellers and the Stationers' Company: they buy assignments from authors, enter copies in the Hall register, and prosecute reprinters in Chancery. They lobbied for the 1710 settlement after the licensing machinery lapsed, and spent the following decades arguing first for a broad statutory construction and then for perpetual common-law property (Millar v Taylor, 1769). Their defining structural position is switching among framings — guild privilege, statutory right, natural property — deploying whichever the current forum rewards.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, london_book_trade, beneficiary,
    organized, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__thinkability_reading, london_book_trade, agenda_setter).

% Buy books at prices carrying a premium above production cost, and receive in return a steady supply of new titles plus works released to the commons as terms expire. Diffuse and unorganized, they hold no seat in the settlement, the register, or Chancery; cheap Scottish reprints serve their interests de facto, arriving through the reprinters' litigation rather than through any voice of their own.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, reading_public, payer,
    powerless, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__thinkability_reading, reading_public, beneficiary).

% Translators, abridgers, compilers, and new writers who build on existing expression: they must clear rights or face Chancery injunctions, yet the same category titles them as holders of their own new works. Their raw material arrives encumbered; their outputs become assets. Exit would mean confining themselves to matter the category leaves free — ideas, facts, ancient texts — a boundary the courts themselves draw.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, subsequent_creators, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(ip_category_emergence__thinkability_reading, subsequent_creators, beneficiary).

% The Scottish and provincial reprinting trades, outside the London settlement: they reprint popular works cheaply and sell into the English market. They would have argued for free reprinting of published works as the default, but held no seat where the settlement's terms were made; they enter the record as defendants in Chancery and, after the House of Lords rejects perpetual right in 1774, as the side whose position is vindicated.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, provincial_scottish_printers, excluded,
    organized, biographical, constrained, national).

% The Court of Chancery and the common-law courts administer the category: they grant or refuse injunctions, decide what 'copy right' covers, and in Millar v Taylor and Donaldson v Becket determine whether the claim is perpetual property or a limited statutory bargain. Once the category exists, every text dispute arrives at them already framed in its vocabulary; they cannot adjudicate outside it.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, chancery_common_law_courts, agenda_setter,
    institutional, generational, constrained, national).

% Enacted the 1710 settlement: it broke the guild monopoly's remaining legal basis, created the author-titled limited term with a renewal, and required registration at Stationers' Hall. It retains the power to restructure or abolish the category by statute, but the trade's support for the settlement and the learning-encouragement framing make wholesale removal politically prohibitive.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, parliament, agenda_setter,
    institutional, generational, constrained, national).

% Jurists and later historians of the book — from Blackstone's commentaries to the modern historiography of the Statute — describe the category's emergence, distinguish it from guild privilege, and supply the vocabulary in which the 1710 transition is argued. They collect no revenue and bear no duties under the category; their seat is analytical.
narrative_ontology:constraint_stakeholder(ip_category_emergence__thinkability_reading, legal_doctrinal_commentators, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(ip_category_emergence__thinkability_reading, london_book_trade).
narrative_ontology:fixing_cost_class(ip_category_emergence__thinkability_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Standardizes the claim-form for intangible expression: after 1710, 'copy right' is a single, registrable, transferable, litigable property claim — authors can convey it, the trade can buy and enforce it, and courts can adjudicate it — replacing the patchwork of guild privilege, crown licensing, and physical-copy possession. It also coordinates public expectations: which texts may be reprinted, for how long, and when they fall to the commons.
% TRANSFER_FUNCTION: Moves exclusive control over the reproduction and vending of expression — and the access revenue attached to it — from the reading public and subsequent creators to rights-holders (authors and, in practice, their assignees in the book trade) for the statutory term; moves works into the public commons at term's end.
% ABSENT_VOICES: The Scottish and provincial reprinting trades would have argued for free reprinting of published works as the default; organized readers, who benefited from cheap reprints, had no seat at all; and no one spoke for subsequent creators as a class. The settlement's 'encouragement of learning' framing was authored by the trade and Parliament; the parties who would bear the access premiums and clearance burdens were not in the conversation that set its terms.
% DISAPPEARANCE_RATIONALE: If the category vanished overnight, the rights market would dissolve — no conveyable 'copy right,' no register, no Chancery injunctions — and the book trade would rearrange around patronage, subscription, and physical-copy margins; the enforcement war between London and the reprinting trades would lose its object; works already released at term would stay common, but new expression would carry no encumbrance to clear. The publishing economy as constituted by the category is what would rearrange.
% FOUNDING_PROBLEM: The lapse of the Licensing Act in 1695 left the London book trade without enforcement against Scottish and provincial piracy while the guild monopoly's legal basis was politically dead; Parliament needed a settlement that secured continued book production ('encouragement of learned men to compose and write useful books') without restoring crown-licensed privilege.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set by the book-trade historiography (Feather, Rose, Deazley, Alexander) and by the statutory record itself — the settlement's trade provisions (grandfathering of existing copies, registration at Stationers' Hall) are legible independently of the learning-encouragement preamble. No organized voice corroborates it from the reading public's side; that seat remains unrepresented, which is itself signal.
narrative_ontology:disappearance_verdict(ip_category_emergence__thinkability_reading, world_rearranges).
narrative_ontology:founding_problem_status(ip_category_emergence__thinkability_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ip_category_emergence__thinkability_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(ip_category_emergence__thinkability_reading, 'none', 1).
narrative_ontology:epsilon_provenance(ip_category_emergence__thinkability_reading, 0.6, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ip_category_emergence__thinkability_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ip_category_emergence__thinkability_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ip_category_emergence__thinkability_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.6: the category's charge on those it governs is the default-encumbrance of expression — access prices above production cost for readers, clearance burdens for subsequent creators, prosecution for reprinters — damped by the category's own byproducts: the term-expiry commons and the idea/expression line, both of which the same conceptual gain created. Suppression 0.65 is structural, not violent: once 'copy right' exists as the claim-form, privilege framings became legally illegible and commons reprinting became piracy; the register and Chancery hold that boundary. Suppression here is the foreclosure of alternative claim-forms — a raw structural property, unscaled by power or scope. Theater 0.15: the category is functionally load-bearing; the theatrical overlays are the preamble's learning-encouragement cover and the Millar-era ancient-common-law property narrative, the latter stripped by Donaldson. Accessibility_collapse 0.52: alternatives persist only at the category's edges. Resistance 0.55: the reprinting trade's decades-long litigation war, ending in the 1774 vindication. The measurement series runs on one shared grid (1695, 1710, 1725, 1740, 1755, 1769, 1774) with all three tracked metrics authored at every point; its central fact is non-monotonic — extractiveness climbs with the category's consolidation, peaks at Millar v Taylor (1769) when the trade's perpetual-property campaign threatened to make the category a perpetual enclosure, and settles lower at Donaldson (1774) when the Lords confirmed the statutory term. The suppression_requirement series is authored because the story genuinely tracks enforcement-capacity change: collapse of the licensing machinery (1695), build-up of the statutory machinery (1710–1740), peak injunction activity (1769), normalization (1774).
 *
 * PERSPECTIVAL GAP:
 *   From the trade's seat the category is the medium of every text-claim it makes — it cannot experience the category as a burden because it is the frame through which all its interests are expressed; from the reader's and subsequent creator's seats the same frame is a born-encumbrance on material they did not author. The courts' seat is structurally singular: for two decades (Millar to Donaldson) the category's whole shape — perpetual property or limited bargain — sat in their hands, so the administering seat experienced the category as a decision rather than a fact. Same legal order, opposite lived structure: this is the divergence the engine should compute between the beneficiary-administrator, payer, and adjudicating seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to d as follows: london_book_trade and authors sit near the beneficiary end — the trade lowest, since it both collects the access revenue and administers the register; authors somewhat higher, since the category titles them while the trade captures most of the economics. reading_public derives toward the full-target end from its victims-list membership, which would overstate its position: the same category delivers incentivized production and the term-expiry commons to it, so an override sets powerless to 0.7 (the only powerless seat in the story, so the atom-keyed override touches nothing else). subsequent_creators derive high as victims; their secondary holder position is carried by the stakeholder surface rather than an override, because the override mechanism is keyed by power atom and authors share that atom — an override there would drag a genuine beneficiary seat toward the middle. provincial_scottish_printers derive high: they are the enforcement's targets. Courts and parliament are structural seats that administer without collecting; commentators are analytical. Scope is national throughout — the 1710 category is a national statutory creation, so the engine's scope amplification applies to a national-scale verification problem, not a universal one.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both misreadings. Reading the category as pure extraction would erase its real coordination work: the standardized, transferable, litigable claim-form that created markets in expression — and, in the same stroke, created the public domain as a thinkable legal category, which no pre-1710 framing contained. Reading it as pure coordination would erase the asymmetry: the same frame that lets an author convey a right charges every reader a premium and every builder a clearance. Tangled rope holds both halves. On mandatrophy: the founding problem (a post-guild settlement securing book production after the Licensing Act's lapse) is contested rather than resolved — the trade's successors attest the incentive problem lives on in each new medium; the historiography attests the specific 1710 bargain is long dead and the category now persists by expansion far beyond it. That contested status is why the story carries the liveness omega rather than a resolved-mandatrophy flag: the category has not outlived its function so much as outgrown its founding justification — a drift to watch, not a verdict to enter.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is the thinkability_reading of kernel ip_category_emergence; what structurally changes if the first_holding_reading is the right account of 1710?',
    'Side-by-side comparison with the sibling story''s referent, ε, and victim structure: if 1710 marks claimant-set entry rather than category creation, the referent arrangement becomes the pre-existing claim structure that admitted a new claimant, not a new category.',
    'Under the sibling reading, this story''s ε attribution shifts from category-default-encumbrance to claimant-set exclusion, the victim set re-derives from who was excluded from the legitimate-claimant set, and the tangled_rope structure would need re-derivation on occupancy dynamics rather than category boundaries.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of the ip_category_emergence kernel; names what the sibling occupancy reading would change.').

omega_variable(
    seam_collapse_test,
    'Are category-emergence (this reading) and first-holding formally independent events, or does the diachronic framing collapse under synchronic analysis so that the two markers are one event (the M4/M5 collapse test)?',
    'Formal analysis of whether ''the category exists'' and ''a claimant occupies it'' can come apart at the founding moment: if every pre-1710 record that would show first-holding also presupposes the category, and every category marker coincides with occupancy markers, the seam reading wins.',
    'If the seam collapses, this constraint merges with the first-holding story: the distinct referent (a thinkability event separate from an occupancy event) dissolves, and ε must be re-authored on the merged event.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(seam_collapse_test, conceptual, 'Whether the thinkability/first-holding distinction survives the synchronic-diachronic collapse test.').

omega_variable(
    category_vs_privilege_distinctness,
    'Is the post-1710 ''copy right'' structurally distinct from guild privilege, or a relabeled Stationers'' monopoly wearing an author-titled front?',
    'Structural comparison of the statutory machinery against the privilege machinery — limited term with renewal versus perpetuity, author-titled conveyance versus guild-held grant, Hall registration versus crown licensing — with the House of Lords'' 1774 rejection of perpetual common-law right as the decisive test of distinctness.',
    'If relabeling, the thinkability reading fails: ε attribution shifts to a continuity story in which the privilege regime persists in new dress, and the category-emergence claim reduces to a change of administration. If distinct, this reading stands as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_vs_privilege_distinctness, empirical, 'The reading''s core historical claim: distinctness of the new claim-form from the old privilege regime.').

omega_variable(
    natural_vs_constructed_category,
    'Is ownable expression a natural category that law merely recognized in 1710, or a constructed category that the statute created?',
    'Pre-1710 claim-form survey of the litigation, licensing, and trade records: if property-in-expression claims were being made and adjudicated before 1710, the category pre-existed the statute; if the claim-form is absent until the statutory vocabulary appears, the statute created it.',
    'If natural, the category would carry mountain-like immunity and this story''s constructed-category metrics misdescribe it; if constructed (this reading''s claim), the category remains contingent and revisable, and the 1710 date is load-bearing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_category, conceptual, 'Naturalness ambiguity of the ownable-expression category — the mountain-versus-constructed question for this constraint.').

omega_variable(
    founding_problem_liveness,
    'Does the category still solve a live coordination problem, or has the founding problem (a post-guild settlement of the book trade) died while the category persists by expansion and industry maintenance?',
    'Comparative incentive analysis across media: whether production decisions demonstrably depend on the category or on alternatives (patronage, commission, subscription, crowdfunding) that now scale; corroborated from outside the rights-holding industries.',
    'If the founding problem is dead and persistence is maintained by the industries the category now serves, the category drifts toward theatrical maintenance of an atrophied function; if live, the tangled_rope structure holds as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_liveness, empirical, 'Liveness of the founding problem — the mandatrophy question for the 1710 category.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ip_category_emergence__thinkability_reading, 1695, 1774).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ip_cat_thinkability_tr_t1695, ip_category_emergence__thinkability_reading, theater_ratio, 1695, 0.05).
narrative_ontology:measurement_basis(ip_cat_thinkability_tr_t1695, observed).
narrative_ontology:measurement(ip_cat_thinkability_tr_t1710, ip_category_emergence__thinkability_reading, theater_ratio, 1710, 0.25).
narrative_ontology:measurement_basis(ip_cat_thinkability_tr_t1710, observed).
narrative_ontology:measurement(ip_cat_thinkability_tr_t1725, ip_category_emergence__thinkability_reading, theater_ratio, 1725, 0.18).
narrative_ontology:measurement_basis(ip_cat_thinkability_tr_t1725, observed).
narrative_ontology:measurement(ip_cat_thinkability_tr_t1740, ip_category_emergence__thinkability_reading, theater_ratio, 1740, 0.16).
narrative_ontology:measurement_basis(ip_cat_thinkability_tr_t1740, observed).
narrative_ontology:measurement(ip_cat_thinkability_tr_t1755, ip_category_emergence__thinkability_reading, theater_ratio, 1755, 0.2).
narrative_ontology:measurement_basis(ip_cat_thinkability_tr_t1755, observed).
narrative_ontology:measurement(ip_cat_thinkability_tr_t1769, ip_category_emergence__thinkability_reading, theater_ratio, 1769, 0.3).
narrative_ontology:measurement_basis(ip_cat_thinkability_tr_t1769, observed).
narrative_ontology:measurement(ip_cat_thinkability_tr_t1774, ip_category_emergence__thinkability_reading, theater_ratio, 1774, 0.15).
narrative_ontology:measurement_basis(ip_cat_thinkability_tr_t1774, observed).

% Extraction over time
narrative_ontology:measurement(ip_cat_thinkability_be_t1695, ip_category_emergence__thinkability_reading, base_extractiveness, 1695, 0.05).
narrative_ontology:measurement_basis(ip_cat_thinkability_be_t1695, observed).
narrative_ontology:measurement(ip_cat_thinkability_be_t1710, ip_category_emergence__thinkability_reading, base_extractiveness, 1710, 0.42).
narrative_ontology:measurement_basis(ip_cat_thinkability_be_t1710, observed).
narrative_ontology:measurement(ip_cat_thinkability_be_t1725, ip_category_emergence__thinkability_reading, base_extractiveness, 1725, 0.5).
narrative_ontology:measurement_basis(ip_cat_thinkability_be_t1725, observed).
narrative_ontology:measurement(ip_cat_thinkability_be_t1740, ip_category_emergence__thinkability_reading, base_extractiveness, 1740, 0.55).
narrative_ontology:measurement_basis(ip_cat_thinkability_be_t1740, observed).
narrative_ontology:measurement(ip_cat_thinkability_be_t1755, ip_category_emergence__thinkability_reading, base_extractiveness, 1755, 0.62).
narrative_ontology:measurement_basis(ip_cat_thinkability_be_t1755, observed).
narrative_ontology:measurement(ip_cat_thinkability_be_t1769, ip_category_emergence__thinkability_reading, base_extractiveness, 1769, 0.7).
narrative_ontology:measurement_basis(ip_cat_thinkability_be_t1769, observed).
narrative_ontology:measurement(ip_cat_thinkability_be_t1774, ip_category_emergence__thinkability_reading, base_extractiveness, 1774, 0.6).
narrative_ontology:measurement_basis(ip_cat_thinkability_be_t1774, observed).

% Suppression requirement over time
narrative_ontology:measurement(ip_cat_thinkability_su_t1695, ip_category_emergence__thinkability_reading, suppression_requirement, 1695, 0.2).
narrative_ontology:measurement_basis(ip_cat_thinkability_su_t1695, observed).
narrative_ontology:measurement(ip_cat_thinkability_su_t1710, ip_category_emergence__thinkability_reading, suppression_requirement, 1710, 0.45).
narrative_ontology:measurement_basis(ip_cat_thinkability_su_t1710, observed).
narrative_ontology:measurement(ip_cat_thinkability_su_t1725, ip_category_emergence__thinkability_reading, suppression_requirement, 1725, 0.55).
narrative_ontology:measurement_basis(ip_cat_thinkability_su_t1725, observed).
narrative_ontology:measurement(ip_cat_thinkability_su_t1740, ip_category_emergence__thinkability_reading, suppression_requirement, 1740, 0.6).
narrative_ontology:measurement_basis(ip_cat_thinkability_su_t1740, observed).
narrative_ontology:measurement(ip_cat_thinkability_su_t1755, ip_category_emergence__thinkability_reading, suppression_requirement, 1755, 0.68).
narrative_ontology:measurement_basis(ip_cat_thinkability_su_t1755, observed).
narrative_ontology:measurement(ip_cat_thinkability_su_t1769, ip_category_emergence__thinkability_reading, suppression_requirement, 1769, 0.75).
narrative_ontology:measurement_basis(ip_cat_thinkability_su_t1769, observed).
narrative_ontology:measurement(ip_cat_thinkability_su_t1774, ip_category_emergence__thinkability_reading, suppression_requirement, 1774, 0.65).
narrative_ontology:measurement_basis(ip_cat_thinkability_su_t1774, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ip_category_emergence__thinkability_reading, resource_allocation).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, ip_category_emergence__first_holding_reading).
narrative_ontology:affects_constraint(ip_category_emergence__thinkability_reading, ip_category_emergence__synchronic_diachronic_seam).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language label 'what happened to intellectual property in 1710' decomposes, per the ε-invariance principle, into structurally distinct readings of one kernel — category emergence (this file: the thinkability reading), claimant-set occupancy (ip_category_emergence__first_holding_reading), and the meta-question of whether the first two are formally independent or a temporal framing artifact (ip_category_emergence__synchronic_diachronic_seam). Each member carries its own ε, beneficiary/victim structure, and classification; forcing one story to cover all three would make ε observer-dependent. The thinkability reading is upstream: a category must exist before a claimant can occupy it, so this reading's claim conditions what the occupancy reading is about, and both together constitute the seam test's object.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ip_category_emergence__thinkability_reading, powerless, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
